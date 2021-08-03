{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Semant.SemanticAnalyser where

import Control.Monad.State (evalState, get, runState)
import Control.Monad.Writer hiding (Any)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Data.Void
import Debug.Trace (traceShowId)
import Lexer.Combinator.Lexer (lex')
import Lexer.Lexeme (BuiltinType (..), Lexeme (Not))
import Lexer.Token
import Parser.Ast (Construct (ConstructError, FuncDecl, FuncDefn, StructDecl, VarDecl), Expr (..), InfixOp (..), Program (Program), Type (PrimitiveType, StructType), decreasePointerLevel, pointerLevel)
import qualified Parser.Ast as Ast
import Parser.AstVisualiser
import Parser.Combinator.TokenStream
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import Parser.Pratt.Parser (arraySizes, expr, parse, parseExpr, parseExprs)
import Semant.Analysers.ExpressionsAnalyser
import Semant.Analysers.FuncsAnalyser
import Semant.Analysers.StatementsAnalyser
import Semant.Analysers.StructsAnalyser
import Semant.Ast.SemantAst hiding (funcs, globals, structs)
import Semant.Ast.SemantAstVisualiser (visualise, visualiseSemantAst)
import Semant.Env
import qualified Semant.Env hiding (funcs, globals, structs)
import Semant.Errors.SemantError hiding (Void)
import Semant.Errors.SemantErrorBundle (bundleSemantErrors)
import Semant.Operators.Cond ((<||>), (|>), (||>))
import Semant.Semant (structs)
import Semant.Semant hiding (structs)
import Semant.Type
import System.Console.Pretty
import Text.Megaparsec (ParseErrorBundle, Stream)

analyseExpr' :: Ast.Expr -> Either [SemantError] SExpr
analyseExpr' expr = case result of
  (expr, []) -> Right expr
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (analyseExpr expr)) getBaseEnv

analyseExprStateful' :: Ast.Expr -> Semant.Env.Env -> Either [SemantError] SExpr
analyseExprStateful' expr env = case result of
  (expr, []) -> Right expr
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (analyseExpr expr)) env

analyseStmtStateful' :: Ast.Statement -> Semant.Env.Env -> Either [SemantError] SStatement
analyseStmtStateful' stmt env = case result of
  (stmt, []) -> Right stmt
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (analyseStatement stmt)) env

analyseProg' :: Ast.Program -> Either [SemantError] SProgram
analyseProg' prog = case result of
  (prog, []) -> Right prog
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (analyse prog)) getBaseEnv

analyseProg :: String -> String -> [Token] -> Ast.Program -> Env -> Either (ParseErrorBundle TokenStream Void, String) (SProgram, Env)
analyseProg file input tokens prog env = case result of
  (prog, []) -> Right (fst result, env')
  (_, errors) -> Left (bundleSemantErrors file tokens errors, input)
  where
    (result, env') = runState (runWriterT (analyse prog)) env

analyseProgs :: [(String, String, [Token], Ast.Program)] -> Env -> Either (ParseErrorBundle TokenStream Void, String) [SProgram]
analyseProgs [(file, input, tokens, program)] env = case analyseProg file input tokens program env of
  (Left errors) -> Left errors
  (Right success) -> Right [analysedProg success]
  where
    analysedProg = fst
analyseProgs ((file, input, tokens, program) : programs) env = case analyseProg file input tokens program env of
  (Left errors) -> Left errors
  (Right analysisResult) -> case analyseProgs programs (nextEnv analysisResult) of
    (Left errors) -> Left errors
    (Right analysedProgs) -> Right (analysedProg analysisResult : analysedProgs)
  where
    analysedProg = fst
    nextEnv = snd

runAnalyse' :: String -> String -> IO ()
runAnalyse' file input = do
  isPretty <- supportsPretty
  case lex' file input of
    (Right tokens) -> case parse file tokens of
      (Right prog) -> case fst $ analyseProg'' prog of
        (sast, []) -> do
          putStrLn $ visualiseAst prog
          putStrLn $ visualiseSemantAst sast
        (sast, errors) -> do
          putStrLn $ visualiseSemantAst sast
          print errors
          let displayedErrors = map prettyPrintSemantError errors

          traverse_ putStrLn displayedErrors
      (Left bundle) -> do
        print bundle
        putStrLn $ prettyPrintErrors bundle (pack input) isPretty
    (Left bundle) -> putStrLn $ prettyPrintErrors bundle (pack input) isPretty
  where
    analyseProg'' prog = runState (runWriterT (analyse prog)) getBaseEnv

prettyPrintSemantError :: SemantError -> String
prettyPrintSemantError = renderString . layoutSmart defaultLayoutOptions . pretty

analyse :: Program -> Semant SProgram
analyse (Program _ constrs) = do
  traverse_
    ( \case
        (StructDecl decl) -> void (analyseStructDecl decl)
        (FuncDecl decl) -> void (analyseFuncDecl decl)
        (FuncDefn defn) -> void (analyseFuncDefn defn)
        (VarDecl decl) -> void (processVarDecl decl)
        ConstructError -> return ()
    )
    constrs
  structDefns <- Semant.Semant.structs
  funcDefns <- Semant.Semant.funcs
  globalVarDecls <- globals
  return
    ( SProgram
        (Map.elems structDefns)
        (Map.elems funcDefns)
        (Map.elems globalVarDecls)
    )
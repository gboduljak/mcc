{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Semant.SemanticAnalyser where

import Control.Monad.State (evalState, get)
import Control.Monad.Writer hiding (Any)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Lexer.Combinator.Lexer (lex')
import Lexer.Lexeme (BuiltinType (..), Lexeme (Not))
import Parser.Ast (Construct (ConstructError, FuncDecl, FuncDefn, StructDecl, VarDecl), Expr (..), InfixOp (..), Program (Program), Type (PrimitiveType, StructType), decreasePointerLevel, pointerLevel)
import qualified Parser.Ast as Ast
import Parser.AstVisualiser
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import Parser.Pratt.Parser (arraySizes, expr, parse, parseExpr, parseExprs)
import Semant.Analysers.ExpressionsAnalyser
import Semant.Analysers.FuncsAnalyser
import Semant.Analysers.StatementsAnalyser
import Semant.Analysers.StructsAnalyser
import Semant.Ast.SemantAst hiding (funcs, globals, structs)
import Semant.Ast.SemantAstVisualiser (visualise, visualiseSemantAst)
import qualified Semant.Env hiding (funcs, globals, structs)
import Semant.Errors.SemantError hiding (Void)
import Semant.Operators.Cond ((<||>), (|>), (||>))
import Semant.Semant (structs)
import Semant.Semant hiding (structs)
import Semant.Type
import System.Console.Pretty
import System.Directory.Internal.Prelude (traverse_)

analyseExpr' :: Ast.Expr -> Either [SemantError] SExpr
analyseExpr' expr = case result of
  (expr, []) -> Right expr
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (analyseExpr expr)) getEmptyEnv

analyseExprStateful' :: Ast.Expr -> Semant.Env.Env -> Either [SemantError] SExpr
analyseExprStateful' expr env = case result of
  (expr, []) -> Right expr
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (analyseExpr expr)) env

analyseProg' :: Ast.Program -> Either [SemantError] SProgram
analyseProg' prog = case result of
  (prog, []) -> Right prog
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (analyse prog)) getEmptyEnv

runAnalyse' :: String -> String -> IO ()
runAnalyse' file input = do
  isPretty <- supportsPretty
  case lex' file input of
    (Right tokens) -> case parse file tokens of
      (Right prog) -> case analyseProg'' prog of
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
    analyseProg'' prog = evalState (runWriterT (analyse prog)) getEmptyEnv

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
  structDefns <- structs
  funcDefns <- funcs
  globalVarDecls <- globals
  return
    ( SProgram
        (Map.elems structDefns)
        (Map.elems funcDefns)
        (Map.elems globalVarDecls)
    )
module Semant.Exports 

where 

import qualified Parser.Ast as Ast
import Semant.Errors.SemantError (SemantError (EmptyProgram))
import Semant.Ast.SemantAst (SExpr, SStatement, SProgram)
import Semant.Env
import Lexer.Token (Token)
import Text.Megaparsec (ParseErrorBundle)
import Parser.Combinator.TokenStream (TokenStream)
import Data.Void (Void)
import Control.Monad.State (evalState, runState)
import Control.Monad.Writer (runWriterT)
import Semant.Analysers.StatementsAnalyser (analyseStatement)
import Semant.Analysers.ExpressionsAnalyser as ExprAnalyser  (analyseExpr)
import Semant.Semant ( getBaseEnv, ensureHasMain, runSemant )
import Semant.Errors.SemantErrorBundle (bundleSemantErrors)
import Semant.SemanticAnalyser (analyse)
import System.Console.Pretty ( supportsPretty )
import Lexer.Combinator.Lexer (lex')
import Parser.Pratt.Parser (parse)
import Parser.AstVisualiser (visualiseAst)
import Semant.Ast.SemantAstVisualiser (visualiseSemantAst)
import Data.Foldable (traverse_)
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import Data.Text (pack)
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import Data.Text.Prettyprint.Doc (layoutSmart, defaultLayoutOptions, Pretty (pretty))
import Semant.Type (Type)

analyseExpr :: Ast.Expr -> Either [SemantError] SExpr
analyseExpr expr = case result of
  (expr, []) -> Right expr
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (ExprAnalyser.analyseExpr expr)) getBaseEnv

analyseExprStateful :: Ast.Expr -> Env Type -> Either [SemantError] SExpr
analyseExprStateful expr env = case result of
  (expr, []) -> Right expr
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (ExprAnalyser.analyseExpr expr)) env

analyseStmtStateful :: Ast.Statement -> Env Type -> Either [SemantError] SStatement
analyseStmtStateful stmt env = case result of
  (stmt, []) -> Right stmt
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (analyseStatement stmt)) env

analyseProg :: String -> String -> [Token] -> Ast.Program -> Env Type -> Either (ParseErrorBundle TokenStream Void, String) (SProgram, Env Type)
analyseProg file input tokens prog env = case result of
  (prog, []) -> Right (fst result, env')
  (_, errors) -> Left (bundleSemantErrors file tokens errors, input)
  where
    (result, env') = runState (runWriterT (analyse prog)) env

analyseProgs :: [(String, String, [Token], Ast.Program)] -> Env Type -> Either (ParseErrorBundle TokenStream Void, String) [SProgram]
analyseProgs [] _ = Left (bundleSemantErrors "" [] [EmptyProgram], "")
analyseProgs [(file, input, tokens, program)] env = case analyseProg file input tokens program env of
  (Left errors) -> Left errors
  (Right analysisResult) -> do
    let mainErrors = hasMainErrors $ resultingEnv analysisResult
    if null mainErrors
      then Right [analysedProg analysisResult]
      else Left (bundleSemantErrors "" [] mainErrors, "")
  where
    analysedProg = fst
    resultingEnv = snd
    hasMainErrors env = snd $ evalState (runWriterT ensureHasMain) env

analyseProgs ((file, input, tokens, program) : programs) env = case analyseProg file input tokens program env of
  (Left errors) -> Left errors
  (Right analysisResult) -> case analyseProgs programs (nextEnv analysisResult) of
    (Left errors) -> Left errors
    (Right analysedProgs) -> Right (analysedProg analysisResult : analysedProgs)
  where
    analysedProg = fst
    nextEnv = snd
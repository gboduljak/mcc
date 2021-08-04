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
import Semant.Semant ( getBaseEnv )
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

analyseExpr :: Ast.Expr -> Either [SemantError] SExpr
analyseExpr expr = case result of
  (expr, []) -> Right expr
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (ExprAnalyser.analyseExpr expr)) getBaseEnv

analyseExprStateful :: Ast.Expr -> Env -> Either [SemantError] SExpr
analyseExprStateful expr env = case result of
  (expr, []) -> Right expr
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (ExprAnalyser.analyseExpr expr)) env

analyseStmtStateful :: Ast.Statement -> Env -> Either [SemantError] SStatement
analyseStmtStateful stmt env = case result of
  (stmt, []) -> Right stmt
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (analyseStatement stmt)) env

analyseProg :: String -> String -> [Token] -> Ast.Program -> Env -> Either (ParseErrorBundle TokenStream Void, String) (SProgram, Env)
analyseProg file input tokens prog env = case result of
  (prog, []) -> Right (fst result, env')
  (_, errors) -> Left (bundleSemantErrors file tokens errors, input)
  where
    (result, env') = runState (runWriterT (analyse prog)) env

analyseProgs :: [(String, String, [Token], Ast.Program)] -> Env -> Either (ParseErrorBundle TokenStream Void, String) [SProgram]
analyseProgs [] _ = Left (bundleSemantErrors "" [] [EmptyProgram], "")
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
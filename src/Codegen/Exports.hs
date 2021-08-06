module Codegen.Exports 


where 

import System.Console.Pretty
import Lexer.Combinator.Lexer (lex')
import Parser.Pratt.Parser (parse)
import Parser.AstVisualiser
import Semant.Exports (prettyPrintSemantError)
import Data.Foldable (traverse_)
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import Data.Text (pack)
import Control.Monad.State (runState)
import Control.Monad.Writer (runWriterT)
import Semant.SemanticAnalyser (analyse)
import Semant.Semant (getBaseEnv)
import Semant.Ast.SemantAstVisualiser (visualiseSemantAst)
import LLVM.Pretty (ppllvm)
import Codegen.Compiler (compile)
import Data.Text.Lazy (unpack)

runCgen :: String -> String -> IO ()
runCgen file input = do
  isPretty <- supportsPretty
  case lex' file input of
    (Right tokens) -> case parse file tokens of
      (Right prog) -> case fst $ analyseProg'' prog of
        (sast, []) -> do
          putStrLn $ visualiseAst prog
          putStrLn $ visualiseSemantAst sast
          print  (compile file sast)
          putStrLn $ unpack $ ppllvm (compile file sast)

        (sast, errors) -> do
          putStrLn $ visualiseSemantAst sast
          let displayedErrors = map prettyPrintSemantError errors

          traverse_ putStrLn displayedErrors
      (Left bundle) -> do
        print bundle
        putStrLn $ prettyPrintErrors bundle (pack input) isPretty
    (Left bundle) -> putStrLn $ prettyPrintErrors bundle (pack input) isPretty
  where
    analyseProg'' prog = runState (runWriterT (analyse prog)) getBaseEnv

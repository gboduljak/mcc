{-# LANGUAGE BlockArguments #-}

module SemanticAnalyserSpec where

import Data.Either
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import GHC.IO.Buffer (checkBuffer)
import GHC.Read (readField)
import qualified Lexer.Combinator.Lexer as CombinatorLexer (lex')
import Lexer.Lexeme (BuiltinType (..), Lexeme)
import Lexer.Token (Token (lexeme))
import Parser.Ast as Ast
import Parser.AstPrettyPrinter
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import qualified Parser.Pratt.Parser as PrattParser (parse)
import Preprocessor.IncludesPreprocessor (preprocess)
import Semant.Ast.SemantAst as SAst hiding (funcs, structs)
import Semant.Env (Env (bindingLoc))
import Semant.Errors.SemantError (BindingLoc (..), SemantError)
import SymbolTable.Scope (Scope (..), rootScopeId, symbolTable)
import Semant.Semant (getBaseEnv)
import Semant.Exports (analyseExpr, analyseExprStateful, analyseProg, analyseProgs)
import Semant.Type
import System.Console.Pretty (supportsPretty)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension)
import Test.Hspec (Spec, SpecWith, describe, expectationFailure, it, shouldBe)
import TestCases
  ( dynamicProgrammingPrograms,
    miniPrograms,
    sortingPrograms,
  )
import Prelude hiding (id, lex)

miniProgramsSpec :: Spec
miniProgramsSpec = do
  describe "semantically analysing mini programs ..." $ do
    mapM_ analyseProgram miniPrograms

sortingSpec :: Spec
sortingSpec = do
  describe "semantically analysing sorting programs ..." $ do
    mapM_ analyseProgram sortingPrograms

dynamicProgrammingSpec :: Spec
dynamicProgrammingSpec = do
  describe "semantically analysing dynamic programming programs ..." $ do
    mapM_ analyseProgram dynamicProgrammingPrograms

ticTacToeSpec :: Spec
ticTacToeSpec =
  describe "semantically analysing tic tac toe program ..." $ do
    analyseProgram "./test/tests-cases/tictactoe/"

analyseProgram :: String -> SpecWith ()
analyseProgram folder = it ("semantically analyses program " ++ folder ++ "...") $ do
  supportsFancyErrors <- supportsPretty
  files <- getDirectoryContents folder
  let headers = [file | file <- files, takeExtension file == ".h"]
      sources = [file | file <- files, takeExtension file == ".c"]
      order = headers ++ sources
  ast <-
    mapM
      ( \file -> do
          input <- readFile (folder ++ file)
          let tokens = lexCombinator "" $ input
          let ast = parse tokens
          return (file, input, tokens, ast)
      )
      order
  case analyseProgs ast getBaseEnv of
    (Left (errors, input)) -> do
      expectationFailure ("expected successfull semantic analysis of " ++ show folder)
    (Right e) -> True `shouldBe` True

prettyPrintSemantError :: SemantError -> String
prettyPrintSemantError = renderString . layoutSmart defaultLayoutOptions . pretty

parse :: [Token] -> Ast.Program
parse tokens = case PrattParser.parse "" tokens of
  (Left errors) -> error "unexpected parse error"
  (Right ast) -> ast

lexCombinator :: String -> String -> [Token]
lexCombinator file input = case result of
  (Left errors) -> []
  (Right tokens) -> tokens
  where
    result = CombinatorLexer.lex' file input
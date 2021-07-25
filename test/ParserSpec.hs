module ParserSpec where

import Data.Foldable (traverse_)
import Data.Text (pack)
import qualified Lexer.Combinator.Lexer as CombinatorLexer (lex')
import Lexer.Lexeme (Lexeme)
import Lexer.Token (Token (lexeme))
import qualified Parser.Combinator.Naive.Parser as CombinatorParser (parse)
import qualified Parser.Combinator.Predictive.Parser as PredictiveCombinatorParser (parse)
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import qualified Parser.Generated.Parser as GeneratedParser (parse)
import qualified Parser.Pratt.Parser as PrattParser (parse)
import System.Console.Pretty (supportsPretty)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension)
import Test.Hspec (Spec, SpecWith, describe, expectationFailure, it, shouldBe)
import TestCases
  ( dynamicProgrammingPrograms,
    miniPrograms,
    sortingPrograms,
  )

miniProgramsSpec :: Spec
miniProgramsSpec = do
  describe "parsing mini programs ..." $ do
    mapM_ parseProgram miniPrograms

sortingSpec :: Spec
sortingSpec = do
  describe "parsing sorting programs ..." $ do
    mapM_ parseProgram sortingPrograms

dynamicProgrammingSpec :: Spec
dynamicProgrammingSpec = do
  describe "parsing dynamic programming programs ..." $ do
    mapM_ parseProgram dynamicProgrammingPrograms

ticTacToeSpec :: Spec
ticTacToeSpec =
  describe "parsing tic tac toe program ..." $ do
    parseProgram "./test/tests-cases/tictactoe/"

parseProgram :: String -> SpecWith ()
parseProgram folder = it ("parses program " ++ folder ++ "...") $ do
  files <- getDirectoryContents folder
  let filesToLex = [file | file <- files, takeExtension file `elem` [".c", ".h"]]

  traverse_
    ( \file -> do
        putStrLn ("\t parsing " ++ file ++ "...")
        input <- readFile (folder ++ file)
        let tokens = lexCombinator file input
        let astParsedWithGen = GeneratedParser.parse (map lexeme tokens)
        let parsedWithComb = CombinatorParser.parse input tokens
        let parsedWithPredComb = PredictiveCombinatorParser.parse input tokens
        let parsedWithPratt = PrattParser.parse file tokens
        case parsedWithComb of
          (Left error) -> do
            printParseErrors "naive combinator" error input
            expectationFailure ("with naive combinator, expected successfull parse of " ++ file)
          (Right ast) -> do
            ast `shouldBe` astParsedWithGen
        case parsedWithPredComb of
          (Left error) -> do
            printParseErrors "predictive combinator" error input
            expectationFailure ("with predictive combinator, expected successfull parse of " ++ file)
          (Right ast) -> do
            ast `shouldBe` astParsedWithGen
        case parsedWithPratt of
          (Left error) -> do
            printParseErrors "pratt" error input
            expectationFailure ("with pratt, expected successfull parse of " ++ file)
          (Right ast) -> do
            ast `shouldBe` astParsedWithGen
    )
    filesToLex

printParseErrors parser errors input = do
  putStrLn $ "Parser " ++ parser ++ " failed with: "
  pretty <- supportsPretty
  let errorMessages = prettyPrintErrors errors (pack input) pretty
   in do
        putStrLn errorMessages

lexCombinator :: String -> String -> [Token]
lexCombinator file input = case result of
  (Left errors) -> []
  (Right tokens) -> tokens
  where
    result = CombinatorLexer.lex' file input
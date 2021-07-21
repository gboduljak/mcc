module ParserSpec where

import Data.Foldable (traverse_)
import qualified Lexer.Combinator.Lexer as CombinatorLexer (lex')
import Lexer.Lexeme (Lexeme)
import Lexer.Token (Token (lexeme))
import qualified Parser.Combinator.Parser as CombinatorParser (parse)
import qualified Parser.Generated.Parser as GeneratedParser (parse)
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
        case parsedWithComb of
          (Left error) -> expectationFailure ("expected successfull parse of " ++ file)
          (Right ast) -> astParsedWithGen `shouldBe` ast
    )
    filesToLex

lexCombinator :: String -> String -> [Token]
lexCombinator file input = case result of
  (Left errors) -> []
  (Right tokens) -> tokens
  where
    result = CombinatorLexer.lex' file input
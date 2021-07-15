module LexerSpec where

import Data.Foldable (traverse_)
import qualified Lexer.AdHoc.Lexer as AdHoc (lex')
import qualified Lexer.Combinator.Lexer as Combinator (lex')
import qualified Lexer.Generator.Lexer as Generator (lex')
import Lexer.Lexeme (Lexeme)
import Lexer.Token (Token (lexeme))
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension)
import Test.Hspec (Spec, SpecWith, describe, expectationFailure, it, shouldBe)

sortingSpec :: Spec
sortingSpec = do
  describe "lexing sorting programs ..." $ do
    mapM_ program sortingPrograms

dynamicProgrammingSpec :: Spec
dynamicProgrammingSpec = do
  describe "lexing dynamic programming programs ..." $ do
    mapM_ program dynamicProgrammingPrograms

ticTacToeSpec :: Spec
ticTacToeSpec =
  describe "lexing tic tac toe program ..." $ do
    program "./test/tests-cases/tictactoe/"

program :: String -> SpecWith ()
program folder = it ("lexes program " ++ folder ++ "...") $ do
  files <- getDirectoryContents folder
  let filesToLex = [file | file <- files, takeExtension file `elem` [".c", ".h"]]

  traverse_
    ( \file -> do
        putStrLn ("\t lexing " ++ file ++ "...")
        input <- readFile (folder ++ file)
        let lexedWithComb = lexCombinator file input
        let lexedWithAdHoc = lexAdHoc file input
        let lexedWithGen = lexGenerator input
        lexedWithComb `shouldBe` lexedWithAdHoc
        lexedWithComb `shouldBe` lexedWithGen
    )
    filesToLex

lexCombinator :: String -> String -> Maybe [Lexeme]
lexCombinator file input = case result of
  (Left errors) -> Nothing
  (Right tokens) -> Just (map lexeme tokens)
  where
    result = Combinator.lex' file input

lexAdHoc :: String -> String -> Maybe [Lexeme]
lexAdHoc file input = case result of
  (Left errors) -> Nothing
  (Right tokens) -> Just (map lexeme tokens)
  where
    result = AdHoc.lex' file input

lexGenerator :: String -> Maybe [Lexeme]
lexGenerator input = Just (Generator.lex' input)

sortingPrograms :: [String]
sortingPrograms =
  [ "./test/tests-cases/sorting/counting-sort/",
    "./test/tests-cases/sorting/insertion-sort/",
    "./test/tests-cases/sorting/merge-sort/",
    "./test/tests-cases/sorting/quick-sort/",
    "./test/tests-cases/sorting/radix-sort/"
  ]

dynamicProgrammingPrograms :: [String]
dynamicProgrammingPrograms =
  [ "./test/tests-cases/dynamic-programming/coin-changing/",
    "./test/tests-cases/dynamic-programming/longest-common-subsequence/"
  ]
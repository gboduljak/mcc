module Main where

import Data.Text (pack)
import Lexer.AdHoc.Lexer (runLex)
import Lexer.Combinator.Lexer (lex')
import Lexer.Generator.Lexer (lex)
import Preprocessor.IncludesPreprocessor (preprocess)
import Preprocessor.IncludesVisualiser (draw)
import System.Environment (getArgs)
import Prelude hiding (lex)

main :: IO ()
main = do
  args <- getArgs
  ((result, graph), _) <- preprocess args
  putStrLn "Constructing dependency graph ... "
  putStrLn "Dependency graph is : "
  putStrLn (draw graph)

  case result of
    (Left errors) -> do
      putStrLn "Compilation halted due to"
      print errors
    (Right order) -> do
      putStrLn ("Compilation order is : " ++ show order)
      let fileToRead = last order
      input <- readFile fileToRead
      let lexRes = lex' input
      putStrLn lexRes
      let lexRes2 = runLex fileToRead (pack input)
      print lexRes2
      let lexRes3 = lex input
      print lexRes3
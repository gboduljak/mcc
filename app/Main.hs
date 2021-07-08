module Main where

import Data.Text (pack)
import Lexer.AdHoc.Lexer (runLex)
import Lexer.Combinator.Lexer (lex')

main :: IO ()
main = do
  file <- getLine
  input <- readFile file
  let lexRes = lex' input
  putStrLn lexRes
  let lexRes2 = runLex file (pack input)
  print lexRes2
  main

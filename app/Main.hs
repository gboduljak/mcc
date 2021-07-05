module Main where

import Lexer.Combinator.Lexer (lex')

main :: IO ()
main = do
  file <- getLine
  input <- readFile file
  let lexRes = lex' input
  putStrLn lexRes
  main

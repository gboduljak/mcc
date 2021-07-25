module Parser.Pratt.Combinators.Prim where

import Lexer.Lexeme (Lexeme (Eof))
import Lexer.Token (Token)
import Parser.Pratt.Combinators.When ((>?))
import Parser.Pratt.Prim (Parser, advance, expect, head, lookahead)
import Prelude hiding (head)

sepBy1 :: Parser a -> (Lexeme -> Bool) -> Lexeme -> Parser [a]
sepBy1 p startsP sep = do x <- p; xs <- accum [x]; return (reverse xs)
  where
    accum xs = do
      lexeme <- lookahead
      if lexeme == sep
        then do
          advance
          (>?)
            [ (startsP, do x <- p; accum (x : xs)),
              (const True, return xs)
            ]
        else do return xs

sepBy :: Parser a -> (Lexeme -> Bool) -> Lexeme -> Parser [a]
sepBy p startsP sep = do
  lexeme <- lookahead
  if startsP lexeme
    then sepBy1 p startsP sep
    else return []

many1 :: Parser a -> (Lexeme -> Bool) -> Parser [a]
many1 p startsP = do x <- p; xs <- accum [x]; return (reverse xs)
  where
    accum xs = do
      lexeme <- lookahead
      if startsP lexeme
        then do
          (>?)
            [ (startsP, do x <- p; accum (x : xs)),
              (const True, return xs)
            ]
        else do return xs

many :: Parser a -> (Lexeme -> Bool) -> Parser [a]
many p startsP = do
  lexeme <- lookahead
  if startsP lexeme
    then many1 p startsP
    else return []

takeWhile :: (Lexeme -> Bool) -> Parser [Token]
takeWhile pred = do
  xs <- takeWhile' []
  return (reverse xs)
  where
    takeWhile' xs =
      (>?)
        [ (\lexeme -> lexeme /= Eof && pred lexeme, do x <- head; advance; takeWhile' (x : xs)),
          (const True, return xs)
        ]
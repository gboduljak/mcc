module Lexer.AdHoc.LexError where

import Data.Text (Text)
import Lexer.TokenPos (TokenPos)

data LexError
  = UnexpectedChar
      { pos :: TokenPos,
        unexpectedChar :: Char,
        expected :: [String]
      }
  | UnexpectedEof
      { pos :: TokenPos,
        expected :: [String]
      }
  deriving (Show)
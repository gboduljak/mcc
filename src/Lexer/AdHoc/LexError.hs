module Lexer.AdHoc.LexError where

import Data.Text (Text)
import Lexer.TokenPos (TokenPos)

data LexError = Unexpected
  { pos :: TokenPos,
    unexpectedChar :: Char,
    expected :: [String]
  }
  deriving (Show)
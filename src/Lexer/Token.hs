module Lexer.Token where

import Data.Text (Text)
import Lexer.Lexeme (Lexeme)
import Lexer.TokenPos (TokenPos)

data Token = Token
  { lexeme :: Lexeme,
    pos :: TokenPos
  }
  deriving (Show)
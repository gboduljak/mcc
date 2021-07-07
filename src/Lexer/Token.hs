module Lexer.Token where

import Data.Text (Text)
import Lexer.Lexeme (Lexeme)
import Lexer.TokenPos (TokenPos (col, line))
import Prelude hiding (line)

data Token = Token
  { lexeme :: Lexeme,
    pos :: TokenPos
  }

instance Show Token where
  show tok =
    show (lexeme tok) ++ " pos: "
      ++ show (line (pos tok))
      ++ ":"
      ++ show (col (pos tok))
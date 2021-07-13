module Lexer.Token where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Lexer.Lexeme (Lexeme)
import Lexer.TokenPos (TokenPos (col, line))
import Prelude hiding (line)

data Token = Token
  { lexeme :: Lexeme,
    pos :: TokenPos
  }

instance Show Token where
  show tok = show (lexeme tok)

instance Pretty Token where
  pretty = pretty . lexeme
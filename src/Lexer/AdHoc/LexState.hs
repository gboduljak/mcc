module Lexer.AdHoc.LexState where

import Data.Text (Text)
import Lexer.AdHoc.LexError (LexError)
import Lexer.TokenPos (TokenPos)

data LexState = LexState
  { input :: Text,
    offset :: Int,
    pos :: TokenPos,
    errors :: [LexError]
  }
  deriving (Show)

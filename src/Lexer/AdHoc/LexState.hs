module Lexer.AdHoc.LexState where

import Data.Text (Text)
import Lexer.AdHoc.LexError (LexError)
import Text.Megaparsec (SourcePos)

data LexState = LexState
  { input :: Text,
    offset :: Int,
    pos :: SourcePos,
    errors :: [LexError]
  }
  deriving (Show)

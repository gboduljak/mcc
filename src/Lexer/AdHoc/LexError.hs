module Lexer.AdHoc.LexError where

import Data.Text (Text)
import Text.Megaparsec (SourcePos)

data LexError
  = UnexpectedChar
      { pos :: SourcePos,
        unexpectedChar :: Char,
        expected :: [String]
      }
  | UnexpectedEof
      { pos :: SourcePos,
        expected :: [String]
      }
  deriving (Show)
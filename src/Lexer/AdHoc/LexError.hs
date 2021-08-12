module Lexer.AdHoc.LexError where

import Data.Text (Text)
import Text.Megaparsec (SourcePos)

data LexError
  = UnexpectedChar
      { pos :: SourcePos,
        off :: Int,
        unexpectedChar :: Char,
        expected :: [String]
      }
  | UnexpectedEof
      { pos :: SourcePos,
        off :: Int,
        expected :: [String]
      }
  deriving (Show)
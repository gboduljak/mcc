module Lexer.Token where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Lexer.Lexeme (Lexeme)
import Text.Megaparsec (SourcePos (..))
import Prelude hiding (line)

data Token = Token
  { lexeme :: Lexeme,
    startPos :: SourcePos,
    endPos :: SourcePos,
    length :: Int
  }
  deriving (Eq, Ord)

instance Show Token where
  show tok = show (lexeme tok)

instance Pretty Token where
  pretty = pretty . lexeme
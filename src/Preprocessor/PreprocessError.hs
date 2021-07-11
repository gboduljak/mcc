module Preprocessor.PreprocessError where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

data PreprocessError
  = Cycle [String]
  | NonexistentFile String
  | InvalidExtension String
  | LexError (ParseErrorBundle Text Void)
  deriving (Show)
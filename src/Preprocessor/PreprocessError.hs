module Preprocessor.PreprocessError where

import Data.List (intercalate)
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (ParseErrorBundle)
import Text.Megaparsec.Error (errorBundlePretty)

data PreprocessError
  = Cycle [String]
  | NonexistentFile String
  | InvalidExtension String
  | LexError (ParseErrorBundle Text Void)

instance Show PreprocessError where 
  show (Cycle cycle) = "Unable to construct a topological order of dependency graph induced by includes. \n   Detected a cycle " 
    ++ intercalate " -> " cycle 
    ++ " ."
  show (NonexistentFile file) = "Detected file " ++ file ++ " does not exist."
  show (InvalidExtension file) = "Cannot process " ++ file ++ ". Supported extensions are .c and .h."
  show (LexError bundle) = "Unable to construct a topological order of dependency graph induced by includes. \n Encountered lexical errors: \n" 
    ++ errorBundlePretty bundle
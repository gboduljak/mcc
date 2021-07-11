module Preprocessor.IncludesGraph where

import Data.Map (Map)

type IncludesGraph = Map String [String]
module Preprocessor.IncludesVisualiser where

import Control.Monad.State (State)
import Control.Monad.Writer
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe
import Preprocessor.IncludesGraph

draw :: IncludesGraph -> String
draw graph = intercalate "\n" (["digraph g {"] ++ drawnNodes ++ drawnEdges ++ ["}"])
  where
    drawnNodes = ["   node" ++ show (nodeId key) ++ "[label=" ++ show key ++ "]" | key <- Map.keys graph, isJust $ Map.lookupIndex key graph]
    drawnEdges = ["   node" ++ show (nodeId file) ++ "->" ++ "node" ++ show (nodeId include) | (file, include) <- edges]
    edges = concat [[(file, include) | include <- includes] | (file, includes) <- Map.toList graph]
    nodeId file = case Map.lookupIndex file graph of
      (Just id) -> id
      Nothing -> invalidNodeId
    invalidNodeId = (length . Map.keys) graph + 1
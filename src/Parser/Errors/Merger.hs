module Parser.Errors.Merger where

import Control.Applicative ((<|>))
import Data.List (groupBy)
import qualified Data.Set as E
import Text.Megaparsec (ParseError (FancyError, TrivialError), ShowErrorComponent, Stream (Token), VisualStream, errorOffset, unexpected)
import Text.Megaparsec.Stream (TraversableStream)

mergeErrorsBasedOnPos :: (Stream s, Ord e) => [ParseError s e] -> [ParseError s e]
mergeErrorsBasedOnPos = map mergeErrorsList . groupBy areAtEqualPos
  where
    areAtEqualPos left right = errorOffset left == errorOffset right

mergeErrorsList :: (Stream s, Ord e) => [ParseError s e] -> ParseError s e
mergeErrorsList = foldr1 mergeErrors

-- Inspired by: https://github.com/mrkkrp/megaparsec/blob/master/Text/Megaparsec/Error.hs
mergeErrors ::
  (Stream s, Ord e) =>
  ParseError s e ->
  ParseError s e ->
  ParseError s e
mergeErrors e1 e2 =
  case errorOffset e1 `compare` errorOffset e2 of
    LT -> e2
    EQ ->
      case (e1, e2) of
        (TrivialError s1 u1 p1, TrivialError _ u2 p2) ->
          TrivialError s1 (merge u1 u2) (E.union p1 p2)
        (FancyError {}, TrivialError {}) -> e1
        (TrivialError {}, FancyError {}) -> e2
        (FancyError s1 x1, FancyError _ x2) ->
          FancyError s1 (E.union x1 x2)
    GT -> e1
  where
    merge Nothing Nothing = Nothing
    merge (Just x) Nothing = Just x
    merge Nothing (Just y) = Just y
    merge (Just x) (Just y) = Just (max x y)
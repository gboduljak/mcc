module Parser.Combinator.When where

import Data.List
import qualified Data.List.NonEmpty as Ne
import qualified Data.Set as Set
import Data.Void
import Lexer.Lexeme
import qualified Lexer.Lexeme as L
import qualified Lexer.Token as T
import Parser.Combinator.Prim
import Parser.Combinator.TokenStream
import Text.Megaparsec

when :: [(Lexeme -> Bool, Parser a)] -> [T.Token] -> Parser a
when [] exp =
  do
    look <- lookAhead (try anySingle)
    off <- getOffset
    let unexpected = Tokens (Ne.fromList [look])
    let expected = Set.fromList [Tokens (Ne.fromList (nub exp))]
    failure (Just unexpected) expected
when ((pred, thenP) : xs) exp = do
  look <- lookAhead (try anySingle)
  if pred (T.lexeme look)
    then do thenP
    else do when xs exp

(>?) :: [(Lexeme -> Bool, Parser a)] -> Parser a
(>?) xs = when xs expected
  where
    expected = (map liftToken . filter expectedAny) L.defaultLexemes
    expectedAny x = or [pred x | (pred, _) <- xs]
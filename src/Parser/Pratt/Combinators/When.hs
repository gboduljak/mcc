module Parser.Pratt.Combinators.When where

import Control.Monad.State (gets)
import qualified Data.List.NonEmpty as Ne
import qualified Data.Set as Set
import Lexer.Lexeme (Lexeme)
import qualified Lexer.Lexeme as L
import qualified Lexer.Token as T
import Parser.Pratt.Prim (Parser, ParserState (offset), head, liftToken, lookahead, unexpected)
import Prelude hiding (head)

when :: [(Lexeme -> Bool, Parser a)] -> [T.Token] -> Parser a
when [] exp =
  do
    look <- head
    off <- gets offset
    unexpected look exp
when ((pred, thenP) : xs) exp = do
  look <- lookahead
  if pred look
    then do thenP
    else do when xs exp

(>?) :: [(Lexeme -> Bool, Parser a)] -> Parser a
(>?) xs = when xs expected
  where
    expected = (map liftToken . filter expectedAny) L.defaultLexemes
    expectedAny x = or [pred x | (pred, _) <- xs]
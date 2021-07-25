{-# LANGUAGE NamedFieldPuns #-}

module Parser.Pratt.Combinators.Recovery where

import Control.Monad.Except (catchError)
import Control.Monad.State
import qualified Lexer.Lexeme as L
import Parser.Pratt.Combinators.Prim (takeWhile)
import Parser.Pratt.Prim (Parser, ParserError (..), ParserState (..))
import Prelude hiding (takeWhile)

withRecovery :: Parser a -> (ParserError -> Parser a) -> Parser a
withRecovery p q = p `catchError` q

withFollowsRecovery :: Parser a -> (L.Lexeme -> Bool) -> a -> Parser a
withFollowsRecovery p f q =
  withRecovery
    p
    ( \error -> do
        takeWhile (not . f)
        return q
    )
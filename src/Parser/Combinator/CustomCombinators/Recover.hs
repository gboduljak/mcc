module Parser.Combinator.CustomCombinators.Recover where

import qualified Lexer.Lexeme as L
import qualified Lexer.Token as T
import Parser.Combinator.Prim (Parser)
import Text.Megaparsec
  ( MonadParsec (takeWhile1P, withRecovery),
    registerParseError,
  )

recoverUsingFollows :: Parser a -> (L.Lexeme -> Bool) -> Parser a -> Parser a
recoverUsingFollows p f e = do
  withRecovery
    ( \error -> do
        registerParseError error
        takeWhile1P (Just "") (not . f . T.lexeme)
        e
    )
    p

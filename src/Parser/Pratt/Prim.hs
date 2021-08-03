{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parser.Pratt.Prim where

import Control.Monad.Except
import Control.Monad.State
import Data.List
import qualified Data.List.NonEmpty as Ne
import qualified Data.Set as Set
import Data.Void
import qualified Lexer.Lexeme as L
import qualified Lexer.Token as T
import qualified Parser.Ast as Ast
import Parser.Combinator.TokenStream
import Text.Megaparsec (MonadParsec (lookAhead), ParseError (TrivialError), ParseErrorBundle (ParseErrorBundle), Stream (Tokens), initialPos)
import Text.Megaparsec.Error (ErrorItem (Tokens))

type ParserError = ParseError TokenStream Void

data ParserState = ParserState
  { input :: [T.Token],
    offset :: Int,
    errors :: [ParserError]
  }

type Parser a = ExceptT ParserError (State ParserState) a

getOffset :: Parser Int
getOffset = gets offset

registerParseError :: ParserError -> Parser ()
registerParseError error =
  modify
    ( \ParserState {input, offset, errors} ->
        ParserState
          { input,
            offset,
            errors = error : errors
          }
    )

unexpectedEof :: [T.Token] -> Parser a
unexpectedEof exp = do
  off <- gets offset
  let expected = Set.fromList [Tokens (Ne.fromList (nub exp))]
      error = TrivialError off Nothing expected
  registerParseError error
  throwError error

unexpected :: T.Token -> [T.Token] -> Parser a
unexpected unex exp = do
  off <- gets offset
  let unexpected = Tokens (Ne.fromList [unex])
      expected = Set.fromList [Tokens (Ne.fromList (nub exp))]
      error = TrivialError off (Just unexpected) expected
  registerParseError error
  throwError error

head :: Parser T.Token
head = do
  tokens <- gets input
  case tokens of
    (tok : _) -> return tok
    [] -> unexpectedEof []

lookahead :: Parser L.Lexeme
lookahead = do
  tokens <- gets input
  case tokens of
    (tok : _) -> return (T.lexeme tok)
    [] -> unexpectedEof []

advance :: Parser ()
advance =
  modify
    ( \ParserState {input, offset, errors} ->
        ParserState
          { input = tail input,
            offset = offset + 1,
            errors
          }
    )

expect :: (L.Lexeme -> Bool) -> Parser L.Lexeme
expect pred =
  gets input >>= \case
    [] -> unexpectedEof expected
    tok@T.Token {T.lexeme} : rest -> do
      if pred lexeme
        then advance >> return lexeme
        else unexpected tok expected
  where
    expected = map liftToken . filter pred $ L.defaultLexemes

liftToken :: L.Lexeme -> T.Token
liftToken lexeme = T.Token lexeme pos pos 0
  where
    pos = initialPos ""
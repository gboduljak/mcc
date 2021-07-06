{-# LANGUAGE LambdaCase #-}

module Lexer.AdHoc.Lexer where

import Control.Monad.State (State, gets, modify, runState, when)
import Data.Text
import Lexer.AdHoc.LexError (LexError (Unexpected, pos))
import Lexer.AdHoc.LexState (LexState (LexState, errors, input, offset, pos))
import qualified Lexer.AdHoc.LexState as LexSt
import Lexer.Lexeme
import Lexer.Token (Token (Token))
import qualified Lexer.Token as T
import Lexer.TokenPos (TokenPos (..), next, nextLine)
import Prelude hiding (head, null, tail)

type Lexer a = State LexState a

hasInput :: Lexer Bool
hasInput = gets (not . null . input)

lookAhead :: Lexer (Maybe Char)
lookAhead = do
  hasInput <- hasInput
  if hasInput
    then do
      lookCh <- gets (head . input)
      return (Just lookCh)
    else return Nothing

lookAheadSat :: (Char -> Bool) -> Lexer Bool
lookAheadSat pred = do
  look <- lookAhead
  case look of
    (Just ch) -> return (pred ch)
    _ -> return False

expect :: Char -> Lexer Bool
expect ch = lookAheadSat (== ch)

advance :: Lexer ()
advance = do
  hasInput <- hasInput
  when hasInput $ do
    isNewline <- lookAheadSat (`elem` "\r\n")
    modify (`advancePos` isNewline)
    return ()
  where
    advancePos state isNewline =
      LexState
        { input = tail (input state),
          offset = offset state + 1,
          errors = errors state,
          LexSt.pos = nextPos isNewline (LexSt.pos state)
        }
      where
        nextPos True = nextLine
        nextPos False = next

recoverFrom :: LexError -> Lexer ()
recoverFrom error = do
  modify (reportError error)
  advance
  where
    reportError error state =
      LexState
        { input = input state,
          offset = offset state,
          LexSt.pos = LexSt.pos state,
          errors = errors state ++ [error]
        }

junk :: Lexer ()
junk = return ()

lexeme :: Lexeme -> Lexer Token
lexeme lexeme = do
  currentPos <- gets LexSt.pos
  return (Token lexeme currentPos)

(|?|) :: Maybe a -> (a -> Lexer b) -> Lexer b -> Lexer b
(|?|) look action def = maybe def action look

ands :: Lexer Token
ands =
  do
    advance
    look <- lookAhead
    (|?|)
      look
      ( \case
          '&' -> do
            advance
            lexeme And
          _ -> do lexeme Ampers
      )
      (lexeme Ampers)

scan :: Lexer Token
scan = do
  junk
  currentPos <- gets LexSt.pos
  look <- lookAhead

  (|?|)
    look
    ( \case
        '&' -> ands
        '(' -> advance >> lexeme LParen
        char -> do
          error <- lexeme Error
          recoverFrom (Unexpected currentPos char ["asd"])
          return error
    )
    (lexeme Eof)

lexer :: Lexer [Token]
lexer = do
  tok <- scan

  if T.lexeme tok == Eof
    then return [tok]
    else do
      toks <- lexer
      return (tok : toks)

lex'' :: String -> Text -> Either [LexError] [Token]
lex'' fileName input = case errors state of
  [] -> Right toks
  errors -> Left errors
  where
    (toks, state) = runState lexer initState
    initState = LexState input 0 initPos []
    initPos = TokenPos fileName 0 0

lex''' :: String -> String -> Either [LexError] [Token]
lex''' fileName input = lex'' fileName (pack input)

runLex :: String -> Text -> ([Token], LexState)
runLex fileName input = runState lexer initState
  where
    (toks, state) = runState lexer initState
    initState = LexState input 0 initPos []
    initPos = TokenPos fileName 0 0

runLex' :: String -> String -> ([Token], LexState)
runLex' fileName input = runState lexer initState
  where
    (toks, state) = runState lexer initState
    initState = LexState (pack input) 0 initPos []
    initPos = TokenPos fileName 0 0

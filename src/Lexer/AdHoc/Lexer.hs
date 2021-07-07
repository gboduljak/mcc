{-# LANGUAGE LambdaCase #-}

module Lexer.AdHoc.Lexer where

import Control.Monad.State (MonadState (get, put), State, gets, modify, runState, void, when)
import Data.Char (isAlphaNum, isLetter, isNumber, isSpace)
import Data.Text (Text, head, null, pack, tail)
import Lexer.AdHoc.LexError (LexError (UnexpectedChar, UnexpectedEof, pos))
import Lexer.AdHoc.LexState (LexState (LexState, errors, input, offset, pos))
import qualified Lexer.AdHoc.LexState as LexSt
import Lexer.Lexeme (BuiltinType (Char, Double, Int, Void), Lexeme (..))
import Lexer.Token (Token (Token))
import qualified Lexer.Token as T
import Lexer.TokenPos (TokenPos (..), next, nextLine)
import Prelude hiding (and, head, null, or, tail, takeWhile)

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

isNewline :: Char -> Bool
isNewline x = x `elem` "\r\n"

advance :: Lexer ()
advance = do
  hasInput <- hasInput
  when hasInput $ do
    isNewline <- lookAheadSat isNewline
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

reportError :: LexError -> LexState -> LexState
reportError error state =
  LexState
    { input = input state,
      offset = offset state,
      LexSt.pos = LexSt.pos state,
      errors = errors state ++ [error]
    }

junk :: Lexer ()
junk = void (takeWhile isSpace)

keyword :: String -> Maybe Lexeme
keyword "for" = Just For
keyword "while" = Just While
keyword "if" = Just If
keyword "else" = Just Else
keyword "struct" = Just Struct
keyword "sizeof" = Just Sizeof
keyword "int" = Just (Type Int)
keyword "char" = Just (Type Char)
keyword "double" = Just (Type Double)
keyword "void" = Just (Type Void)
keyword "NULL" = Just LitNull
keyword _ = Nothing

tokenise :: Lexeme -> TokenPos -> Lexer Token
tokenise lexeme pos = return (Token lexeme pos)

tokeniseAndAdvance :: Lexeme -> TokenPos -> Lexer Token
tokeniseAndAdvance x pos = do
  x' <- tokenise x pos
  advance
  return x'

matchOrFallback :: Char -> Lexeme -> Lexeme -> TokenPos -> Lexer Token
matchOrFallback char desired fallback pos = do
  advance
  lookAhead >>= \case
    (Just lookChar) ->
      if lookChar == char
        then tokeniseAndAdvance desired pos
        else tokenise fallback pos
    _ -> tokenise fallback pos

takeWhile :: (Char -> Bool) -> Lexer String
takeWhile pred = do
  lookAhead >>= \case
    (Just x) ->
      if pred x
        then do
          advance
          xs <- takeWhile pred
          return (x : xs)
        else return []
    Nothing -> return []

takeUntil :: (Char -> Bool) -> Lexer String
takeUntil pred = takeWhile (not . pred)

identifierOrKeyword :: Lexer Token
identifierOrKeyword = do
  pos <- gets LexSt.pos
  xs <- takeWhile (\x -> x == '_' || isAlphaNum x)
  case keyword xs of
    (Just lexeme) -> tokenise lexeme pos
    Nothing -> tokenise (Ident xs) pos

integerOrReal :: Lexer Token
integerOrReal = undefined

string :: Lexer Token
string = do
  pos <- gets LexSt.pos
  advance
  chars <- processStringContent
  case chars of
    (Left error@UnexpectedEof {}) -> do
      modify (reportError error)
      tokenise Error pos
    (Left error@UnexpectedChar {}) -> do
      modify (reportError error)
      takeUntil (\x -> x == '\"' || isNewline x)
      tokenise Error pos
    (Right str) -> tokenise (LitString str) pos
  where
    processStringContent = do
      simpleChars <- takeUntil (\x -> x == '\\' || x == '\"' || isNewline x)
      lookAhead
        >>= ( \case
                Nothing -> do
                  pos <- gets LexSt.pos
                  return (Left (UnexpectedEof pos ["string character", "\""]))
                (Just '\"') -> advance >> return (Right simpleChars)
                (Just '\\') -> do
                  advance
                  lookAhead >>= (`handleEscaped` simpleChars)
                (Just newline) -> do
                  pos <- gets LexSt.pos
                  return (Left (UnexpectedChar pos newline ["string character", "\""]))
            )
    handleEscaped lookahead prefix = case lookahead of
      (Just '\\') -> escapeAndProcessRest '\\' prefix
      (Just '"') -> escapeAndProcessRest '\"' prefix
      (Just 'a') -> escapeAndProcessRest '\a' prefix
      (Just 'b') -> escapeAndProcessRest '\b' prefix
      (Just 'f') -> escapeAndProcessRest '\f' prefix
      (Just 'n') -> escapeAndProcessRest '\n' prefix
      (Just 'r') -> escapeAndProcessRest '\r' prefix
      (Just 't') -> escapeAndProcessRest '\t' prefix
      (Just unexpected) -> do
        pos <- gets LexSt.pos
        return (Left (UnexpectedChar pos unexpected escapeable))
      Nothing -> do
        pos <- gets LexSt.pos
        return (Left (UnexpectedEof pos ["string character", "\""]))
    escapeAndProcessRest char prefix = do
      advance
      content <- processStringContent
      case content of
        (Right rest) -> return (Right (prefix ++ (char : rest)))
        error -> return error
    escapeable = ["\\", "\"", "a", "b", "n", "r", "t"]

scan :: Lexer Token
scan = do
  junk
  pos <- gets LexSt.pos
  lookAhead >>= \case
    (Just '-') -> matchOrFallback '>' Arrow Minus pos
    (Just '&') -> matchOrFallback '&' And Ampers pos
    (Just '|') -> matchOrFallback '|' Or Bar pos
    (Just '<') -> matchOrFallback '=' Leq Less pos
    (Just '>') -> matchOrFallback '=' Geq Greater pos
    (Just '=') -> matchOrFallback '=' Equal Assign pos
    (Just '!') -> matchOrFallback '=' Neq Not pos
    (Just ';') -> tokeniseAndAdvance Semi pos
    (Just ',') -> tokeniseAndAdvance Comma pos
    (Just '.') -> tokeniseAndAdvance Dot pos
    (Just '*') -> tokeniseAndAdvance Asterisk pos
    (Just '/') -> tokeniseAndAdvance Div pos
    (Just '%') -> tokeniseAndAdvance Mod pos
    (Just '^') -> tokeniseAndAdvance Caret pos
    (Just '(') -> tokeniseAndAdvance LParen pos
    (Just ')') -> tokeniseAndAdvance RParen pos
    (Just '{') -> tokeniseAndAdvance LBrace pos
    (Just '}') -> tokeniseAndAdvance RBrace pos
    (Just '[') -> tokeniseAndAdvance LBrack pos
    (Just ']') -> tokeniseAndAdvance RBrack pos
    (Just '\"') -> do string
    (Just x) -> do
      if isLetter x || x == '_'
        then do identifierOrKeyword
        else
          if isNumber x
            then do integerOrReal
            else do
              error <- tokenise Error pos
              recoverFrom (UnexpectedChar pos x expected)
              return error
    Nothing -> tokenise Eof pos
  where
    expected = ["number", "operator", "keyword", "identifier"]

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
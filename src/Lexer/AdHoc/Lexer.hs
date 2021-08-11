{-# LANGUAGE LambdaCase #-}

module Lexer.AdHoc.Lexer where

import Control.Monad.State (MonadState (get, put), State, gets, modify, runState, void, when)
import Data.Char (isAlphaNum, isDigit, isLetter, isNumber, isSpace)
import Data.Text (Text, head, null, pack, tail)
import Lexer.AdHoc.LexError (LexError (UnexpectedChar, UnexpectedEof, pos))
import Lexer.AdHoc.LexState (LexState (LexState, errors, input, offset, pos))
import qualified Lexer.AdHoc.LexState as LexSt
import Lexer.Lexeme (BuiltinType (Char, Double, Int, Void), Lexeme (..))
import Lexer.Token (Token (Token))
import qualified Lexer.Token as T
import Text.Megaparsec.Pos
import Utils.CharPredicates
import Prelude hiding (and, head, lex, or, tail, takeWhile)

type Lexer a = State LexState a

lookAhead :: Lexer (Maybe Char)
lookAhead = do
  hasInput <- gets (not . Data.Text.null . input)
  if hasInput
    then do
      lookCh <- gets (head . input)
      return (Just lookCh)
    else return Nothing

advance :: Lexer ()
advance = do
  lookAhead >>= \case
    (Just x) -> do
      modify (`advancePos` isNewline x)
    _ -> return ()
  where
    advancePos state isNewline =
      LexState
        { input = tail (input state),
          offset = offset state + 1,
          errors = errors state,
          LexSt.pos = nextPos isNewline (LexSt.pos state)
        }
      where
        nextPos True = incrementLine
        nextPos False = incrementColumn

        incrementLine (SourcePos name line column) = SourcePos name (line <> pos1) pos1
        incrementColumn (SourcePos name line column) = SourcePos name line (column <> pos1)

unexpectedChar :: Char -> [String] -> Lexer Token
unexpectedChar unexpected expected = do
  pos <- gets LexSt.pos
  recoverFrom (UnexpectedChar pos unexpected expected)
  tokenise Error pos

unexpectedEndOfFile :: [String] -> Lexer Token
unexpectedEndOfFile expected = do
  pos' <- gets LexSt.pos
  recoverFrom (UnexpectedEof pos' expected)
  tokenise Error pos'

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
junk = do
  lookAhead >>= \case
    (Just '/') -> do comment
    (Just x) -> do
      when (isSpace x) $ do
        void (takeWhile isSpace) >> junk
    Nothing -> return ()

comment :: Lexer ()
comment = do
  state <- get
  advance
  lookAhead >>= \case
    (Just '/') -> do
      advance
      takeUntil isNewline
      advance
      junk
    (Just '*') -> multiLineComment >> junk
    _ -> put state

multiLineComment :: Lexer ()
multiLineComment = do
  advance
  takeUntil (== '*')
  lookAhead >>= \case
    (Just '*') -> do
      advance
      lookAhead >>= \case
        (Just '/') -> do
          advance
          return ()
        (Just x) -> multiLineComment
        Nothing -> do
          pos <- gets LexSt.pos
          recoverFrom (UnexpectedEof pos ["/", "character"])
    _ -> do
      pos <- gets LexSt.pos
      recoverFrom (UnexpectedEof pos ["*"])

keyword :: String -> Maybe Lexeme
keyword "for" = Just For
keyword "while" = Just While
keyword "if" = Just If
keyword "else" = Just Else
keyword "struct" = Just Struct
keyword "sizeof" = Just Sizeof
keyword "return" = Just Return
keyword "include" = Just Include
keyword "int" = Just (Type Int)
keyword "char" = Just (Type Char)
keyword "double" = Just (Type Double)
keyword "void" = Just (Type Void)
keyword "NULL" = Just LitNull
keyword _ = Nothing

tokenise :: Lexeme -> SourcePos -> Lexer Token
tokenise lexeme sourcePos = do
  endPos <- gets LexSt.pos
  let lexemeLen = (unPos . sourceColumn) endPos - (unPos . sourceColumn) sourcePos
   in do
        return (Token lexeme sourcePos endPos lexemeLen)

tokeniseAndAdvance :: Lexeme -> SourcePos -> Lexer Token
tokeniseAndAdvance x pos = do
  advance
  tokenise x pos

matchOrFallback :: Char -> Lexeme -> Lexeme -> SourcePos -> Lexer Token
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

identifierOrKeywordOrDirective :: Lexer Token
identifierOrKeywordOrDirective = do
  pos <- gets LexSt.pos
  xs <- takeWhile (\x -> x == '_' || isAlphaNum x)
  case keyword xs of
    (Just lexeme) -> tokenise lexeme pos
    Nothing -> tokenise (Ident xs) pos

integerOrReal :: Lexer Token
integerOrReal = do
  pos <- gets LexSt.pos
  xs <- takeWhile isDigit
  lookAhead >>= \case
    (Just '.') -> do
      advance
      ys <- takeWhile isDigit
      if Prelude.null ys
        then do
          pos <- gets LexSt.pos
          lookAhead >>= \case
            (Just x) -> do
              recoverFrom (UnexpectedChar pos x ["digit"])
              tokenise Error pos
            Nothing -> do
              modify (reportError (UnexpectedEof pos ["digit"]))
              tokenise Error pos
        else do
          tokenise (LitDouble (read (xs ++ "." ++ ys) :: Double)) pos
    _ -> tokenise (LitInt (read xs :: Int)) pos

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
      (Just x) -> do
        if x `elem` concat escapeable
          then do
            processRest (escape x) prefix
          else do
            pos <- gets LexSt.pos
            return (Left (UnexpectedChar pos x escapeable))
      Nothing -> do
        pos <- gets LexSt.pos
        return (Left (UnexpectedEof pos ["string character", "\""]))
    processRest char prefix = do
      advance
      content <- processStringContent
      case content of
        (Right rest) -> return (Right (prefix ++ (char : rest)))
        error -> return error

char :: Lexer Token
char = do
  pos <- gets LexSt.pos
  advance
  lookAhead >>= \case
    (Just '\\') -> do
      advance
      lookAhead >>= \case
        (Just x) -> do
          if x `elem` concat escapeable
            then do
              advance >> endOfChar (escape x) pos
            else do
              unexpectedChar x escapeable
        Nothing -> unexpectedEndOfFile ["\'"]
    (Just '\'') -> unexpectedChar '\'' ["character", "\\"]
    (Just x) -> advance >> endOfChar x pos
    Nothing -> unexpectedEndOfFile ["\'"]
  where
    endOfChar char pos = do
      lookAhead >>= \case
        (Just '\'') -> tokeniseAndAdvance (LitChar char) pos
        (Just x) -> do
          pos' <- gets LexSt.pos
          recoverFrom (UnexpectedChar pos' x ["\'"])
          tokenise Error pos'
        Nothing -> do
          pos' <- gets LexSt.pos
          recoverFrom (UnexpectedEof pos' ["\'"])
          tokenise Error pos'

escape :: Char -> Char
escape '\\' = '\\'
escape '"' = '\"'
escape 'a' = '\a'
escape 'b' = '\b'
escape 'f' = '\f'
escape 'n' = '\n'
escape 'r' = '\r'
escape 't' = '\t'
escape '0' = '\0'
escape x = x

escapeable :: [[Char]]
escapeable = ["\\", "\"", "a", "b", "n", "r", "t", "0"]

include :: Lexer Token
include = do
  advance
  lookAhead >>= \case
    (Just 'i') -> identifierOrKeywordOrDirective
    (Just x) -> unexpectedChar x ["i"]
    Nothing -> unexpectedEndOfFile ["i"]

scan :: Lexer Token
scan = do
  junk
  pos <- gets LexSt.pos
  lookAhead >>= \case
    (Just '#') -> include
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
    (Just '+') -> tokeniseAndAdvance Plus pos
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
    (Just '\"') -> string
    (Just '\'') -> char
    (Just x) -> do
      if isLetter x || x == '_'
        then do identifierOrKeywordOrDirective
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

lex :: String -> Text -> Either [LexError] [Token]
lex fileName input = case errors state of
  [] -> Right toks
  errors -> Left errors
  where
    (toks, state) = runState lexer initState
    initState = LexState input 0 (initialPos fileName) []

lex' :: String -> String -> Either [LexError] [Token]
lex' fileName input = lex fileName (pack input)
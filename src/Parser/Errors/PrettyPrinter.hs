{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser.Errors.PrettyPrinter where

import Control.Monad
import Data.Data (Proxy (Proxy))
import qualified Data.Foldable as E
import qualified Data.Map as M
import qualified Data.Text as T
import System.Console.Pretty
  ( Color (Red),
    Pretty (color, style),
    Style (Bold),
  )
import Text.Megaparsec
  ( ErrorFancy (..),
    ErrorItem (Tokens),
    ParseError (..),
    ParseErrorBundle (..),
    PosState (pstateSourcePos),
    ShowErrorComponent (..),
    SourcePos (sourceColumn, sourceLine),
    Stream (Token),
    TraversableStream (reachOffset),
    VisualStream (tokensLength),
    errorOffset,
    parseErrorTextPretty,
    sourcePosPretty,
    unPos,
  )
import Utils.CharPredicates (isNewline)

-- This module is based on https://github.com/mrkkrp/megaparsec/blob/master/Text/Megaparsec/Error.hs
-- It is a customized versions of the module above. Main difference is a support for colorised error messages
-- and a simplified, possibly slower calculation of error line

prettyPrintErrors ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e,
    Show s
  ) =>
  ParseErrorBundle s e ->
  T.Text ->
  Bool ->
  String
prettyPrintErrors ParseErrorBundle {..} input supportsFancyTerminal = join . reverse $ errorMessages
  where
    (errorMessages, _) = foldl buildErrorMessage ([], bundlePosState) bundleErrors
    buildErrorMessage (!errorMessages, !headState) error = (errorMessage <> "\n" : errorMessages, errorPosState')
      where
        linesByNum = M.fromList (zip [1 ..] (T.split isNewline input))
        (_, errorPosState') = reachOffset (errorOffset error) headState

        errorPos = pstateSourcePos errorPosState'
        errorLineNum = (unPos . sourceLine) errorPos
        offendingLine = maybe "" T.unpack (M.lookup errorLineNum linesByNum)
        errorMessage = prettyErrorMessage offendingLine error errorPos supportsFancyTerminal

prettyErrorMessage ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  String ->
  ParseError s e ->
  SourcePos ->
  Bool ->
  String
prettyErrorMessage inputErrorLine error errorPos isStyled =
  bold isStyled (sourcePosPretty errorPos) <> ":\n"
    <> prettyErrorLine inputErrorLine error errorPos isStyled
    <> parseErrorTextPretty error

prettyErrorLine ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  [Char] ->
  ParseError s e ->
  SourcePos ->
  Bool ->
  String
prettyErrorLine inputErrorLine error errorPos supportsFancyTerminal =
  padding <> "|\n" <> errorLineNum <> " | " <> inputErrorLine
    <> "\n"
    <> padding
    <> "| "
    <> rpadding
    <> red supportsFancyTerminal pointer
    <> "\n"
  where
    errorLen = errorLength error
    errorLineLen = length inputErrorLine
    errorLineNum = (show . unPos . sourceLine) errorPos

    padding = replicate (length errorLineNum + 1) ' '
    rpshift = unPos (sourceColumn errorPos) - 1
    pointerLen =
      if rpshift + errorLen > errorLineLen
        then errorLineLen - rpshift + 1
        else errorLen
    pointer = replicate pointerLen '^'
    rpadding =
      if pointerLen > 0
        then replicate rpshift ' '
        else ""

bold :: Bool -> String -> String
bold True line = style Bold line
bold False line = line

red :: Bool -> String -> String
red True line = color Red line
red False line = line

-- Following copied from :
-- https://github.com/mrkkrp/megaparsec/blob/master/Text/Megaparsec/Error.hs

errorLength ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  ParseError s e ->
  Int
errorLength e =
  case e of
    TrivialError _ Nothing _ -> 1
    TrivialError _ (Just x) _ -> errorItemLength pxy x
    FancyError _ xs ->
      E.foldl' (\a b -> max a (errorFancyLength b)) 1 xs
  where
    pxy = Proxy :: Proxy s

errorItemLength :: VisualStream s => Proxy s -> ErrorItem (Token s) -> Int
errorItemLength pxy = \case
  Tokens ts -> tokensLength pxy ts
  _ -> 1

showErrorFancy :: ShowErrorComponent e => ErrorFancy e -> String
showErrorFancy = \case
  ErrorFail msg -> msg
  ErrorIndentation ord ref actual ->
    "incorrect indentation (got " <> show (unPos actual)
      <> ", should be "
      <> p
      <> show (unPos ref)
      <> ")"
    where
      p = case ord of
        LT -> "less than "
        EQ -> "equal to "
        GT -> "greater than "
  ErrorCustom a -> showErrorComponent a

errorFancyLength :: ShowErrorComponent e => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1
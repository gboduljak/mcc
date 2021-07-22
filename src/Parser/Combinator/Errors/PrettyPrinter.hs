{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser.Combinator.Errors.PrettyPrinter where

import Control.Monad
import Data.Data (Proxy (Proxy))
import qualified Data.Foldable as E
import qualified Data.Map as M
import qualified Data.Text as T
import Debug.Trace
import System.Console.Pretty
  ( Color (..),
    Pretty (color),
    Style (..),
    bgColor,
    style,
    supportsPretty,
  )
import Text.Megaparsec
import Utils.CharPredicates (isNewline)

errorBundlePretty ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  ParseErrorBundle s e ->
  T.Text ->
  IO String
errorBundlePretty ParseErrorBundle {..} input = do
  (errorMessages, _) <- foldM displayError (id, bundlePosState) bundleErrors
  return (errorMessages "")
  where
    displayError (out, !initPosState) error = do
      isStyled <- supportsPretty
      displayedError <- offendingLine

      let errorMessage =
            "\n" <> bold isStyled (sourcePosPretty errorPos) <> ":\n"
              <> displayedError
              <> parseErrorTextPretty error

      return (out . (errorMessage ++), errorPosState')
      where
        linesByNum = M.fromList (zip [1 ..] (T.split isNewline input))
        (_, errorPosState') = reachOffset (errorOffset error) initPosState

        errorPos = pstateSourcePos errorPosState'
        errorLineNum = (unPos . sourceLine) errorPos
        offendingLine =
          case M.lookup errorLineNum linesByNum of
            Nothing -> return ""
            Just errorLine -> prettyErrorLine (T.unpack errorLine) error errorPos

prettyErrorLine ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  [Char] ->
  ParseError s e ->
  SourcePos ->
  IO String
prettyErrorLine inputErrorLine error errorPos = do
  inColor <- supportsPretty

  return
    ( padding <> "|\n" <> errorLineNum <> " | " <> inputErrorLine
        <> "\n"
        <> padding
        <> "| "
        <> rpadding
        <> red inColor pointer
        <> "\n"
    )
  where
    rpadding =
      if pointerLen > 0
        then replicate rpshift ' '
        else ""
    pointerLen =
      if rpshift + errorLen > errorLineLen
        then errorLineLen - rpshift + 1
        else errorLen
    errorLen = errorLength error
    errorLineLen = length inputErrorLine
    errorLineNum = (show . unPos . sourceLine) errorPos

    padding = replicate (length errorLineNum + 1) ' '
    rpshift = unPos (sourceColumn errorPos) - 1
    pointer = replicate pointerLen '^'

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

-- | Pretty-print an 'ErrorFancy'.
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

-- | Get length of the “pointer” to display under a given 'ErrorFancy'.
errorFancyLength :: ShowErrorComponent e => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1
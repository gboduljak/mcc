{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser.Combinator.ErrorBundlePrettyPrinter where

import Data.Data
import qualified Data.Foldable as E
import Data.List (unfoldr)
import safe Text.Megaparsec
  ( ErrorFancy (ErrorCustom),
    ErrorItem (Tokens),
    ParseError (..),
    ParseErrorBundle (..),
    PosState (pstateSourcePos),
    ShowErrorComponent (errorComponentLen),
    SourcePos (sourceColumn, sourceLine),
    Stream (Token),
    TraversableStream (reachOffset),
    VisualStream (tokensLength),
    errorOffset,
    parseErrorTextPretty,
    sourcePosPretty,
    unPos,
  )

separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep
  where
    sep [] = Nothing
    sep l = Just . fmap (drop 1) . break (== chr) $ l

errorBundlePretty ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  -- | Parse error bundle to display
  ParseErrorBundle s e ->
  -- | Textual rendition of the bundle
  String
errorBundlePretty ParseErrorBundle {..} =
  let (r, _) = foldl f (id, bundlePosState) bundleErrors
   in drop 1 (r "")
  where
    f ::
      (ShowS, PosState s) ->
      ParseError s e ->
      (ShowS, PosState s)
    f (o, !pst) e = (o . (outChunk ++), pst')
      where
        (msline, pst') = reachOffset (errorOffset e) pst
        epos = pstateSourcePos pst'
        outChunk =
          "\n" <> sourcePosPretty epos <> ":\n"
            <> offendingLine
            <> parseErrorTextPretty e
        offendingLine =
          case msline of
            Nothing -> ""
            Just sline ->
              let rpadding =
                    if pointerLen > 0
                      then replicate rpshift ' '
                      else ""
                  pointerLen =
                    if rpshift + elen > slineLen
                      then slineLen - rpshift + 1
                      else elen
                  pointer = replicate pointerLen '^'
                  lineNumber = (show . unPos . sourceLine) epos
                  padding = replicate (length lineNumber + 1) ' '
                  rpshift = unPos (sourceColumn epos) - 1
                  slineLen = length sline
               in padding <> "|\n" <> lineNumber <> " | " <> sline
                    <> "\n"
                    <> padding
                    <> "| "
                    <> rpadding
                    <> pointer
                    <> "\n"
        pxy = Proxy :: Proxy s
        elen =
          case e of
            TrivialError _ Nothing _ -> 1
            TrivialError _ (Just x) _ -> errorItemLength pxy x
            FancyError _ xs ->
              E.foldl' (\a b -> max a (errorFancyLength b)) 1 xs

errorItemLength :: VisualStream s => Proxy s -> ErrorItem (Token s) -> Int
errorItemLength pxy = \case
  Tokens ts -> tokensLength pxy ts
  _ -> 1

-- | Get length of the “pointer” to display under a given 'ErrorFancy'.
errorFancyLength :: ShowErrorComponent e => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1
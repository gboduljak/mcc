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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser.Combinator.TokenStream where

import Control.Exception
import Data.Data
import qualified Data.Foldable as E
import Data.List
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Set as Set
import Data.Void
import Debug.Trace
import qualified Lexer.Lexeme as L
import qualified Lexer.Token as T
import Text.Megaparsec
  ( ErrorFancy (..),
    ErrorItem (..),
    ParseError (..),
    ParseErrorBundle (..),
    PosState (..),
    ShowErrorComponent (..),
    SourcePos (SourcePos, sourceColumn, sourceLine),
    Stream (..),
    TraversableStream (reachOffset),
    VisualStream (..),
    errorOffset,
    parseErrorTextPretty,
    sourcePosPretty,
    unPos,
  )
import Text.Megaparsec.Stream

data TokenStream = TokenStream
  { tokenStreamInput :: String, -- for showing offending lines
    unTokenStream :: [T.Token]
  }
  deriving (Show)

instance Stream TokenStream where
  type Token TokenStream = T.Token
  type Tokens TokenStream = [T.Token]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream str (t : ts)) =
    Just
      ( t,
        TokenStream (drop (tokensLength pxy (t :| [])) str) ts
      )
  takeN_ n (TokenStream str s)
    | n <= 0 = Just ([], TokenStream str s)
    | null s = Nothing
    | otherwise =
      let (x, s') = splitAt n s
       in case NE.nonEmpty x of
            Nothing -> Just (x, TokenStream str s')
            Just nex -> Just (x, TokenStream (drop (tokensLength pxy nex) str) s')
  takeWhile_ f (TokenStream str s) =
    let (x, s') = DL.span f s
     in case NE.nonEmpty x of
          Nothing -> (x, TokenStream str s')
          Just nex -> (x, TokenStream (drop (tokensLength pxy nex) str) s')

instance VisualStream TokenStream where
  showTokens Proxy =
    intercalate ", "
      . NE.toList
      . fmap (L.display . T.lexeme)
  tokensLength Proxy xs = sum (T.length <$> xs)

instance TraversableStream TokenStream where
  reachOffsetNoLine o PosState {..} =
    PosState
      { pstateInput =
          TokenStream
            { tokenStreamInput = intercalate ", " . map (L.display . T.lexeme) $ remainingToks,
              unTokenStream = traceShowId remainingToks
            },
        pstateOffset = max pstateOffset o,
        pstateSourcePos = newSourcePos,
        pstateTabWidth = pstateTabWidth,
        pstateLinePrefix = pstateLinePrefix -- fix this
      }
    where
      currentTokOffset = o - pstateOffset
      allInputTokens = unTokenStream pstateInput
      newSourcePos =
        case remainingToks of
          [] -> pstateSourcePos
          (nextTok : _) -> T.startPos nextTok
      (_, remainingToks) = splitAt currentTokOffset allInputTokens

pxy :: Proxy TokenStream
pxy = Proxy

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Parser.Combinator.TokenStream where

import Data.List
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Set as Set
import Data.Void
import qualified Lexer.Lexeme as L
import qualified Lexer.Token as T
import Text.Megaparsec

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
  reachOffset o PosState {..} =
    ( Just $ prefix ++ restOfLine,
      PosState
        { pstateInput =
            TokenStream
              { tokenStreamInput = postStr,
                unTokenStream = post
              },
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
        }
    )
    where
      prefix =
        if sameLine
          then pstateLinePrefix ++ preStr
          else preStr
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> pstateSourcePos
          (x : _) -> T.startPos x
      (pre, post) = splitAt (o - pstateOffset) (unTokenStream pstateInput)
      (preStr, postStr) = splitAt tokensConsumed (tokenStreamInput pstateInput)
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy TokenStream
pxy = Proxy

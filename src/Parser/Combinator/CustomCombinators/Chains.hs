module Parser.Combinator.CustomCombinators.Chains where

import Control.Applicative (Alternative ((<|>)))
import qualified Lexer.Lexeme as L
import Parser.Combinator.Prim

chainl1 :: (Alternative m, Monad m) => m t -> m (t -> t -> t) -> m t
chainl1 p op = do left <- p; rest left
  where
    rest left =
      do
        f <- op
        right <- p
        rest (f left right)
        <|> return left

chainr1 :: (Alternative m, Monad m) => m t -> m (t -> t -> t) -> m t
chainr1 p op = do left <- p; rest left
  where
    rest left = do
      do
        f <- op
        right <- chainr1 p op
        return (f left right)
        <|> return left

chainl1' :: (Alternative m, Monad m) => m b -> m (b -> b) -> m b
chainl1' p op = do left <- p; rest left
  where
    rest left =
      do
        f <- op
        rest (f left)
        <|> return left

lookchainr1 :: Parser a -> [(L.Lexeme -> Bool, Parser (a -> a -> a))] -> Parser a
lookchainr1 p opts = do left <- p; rest left
  where
    rest left =
      look >>= \lexeme -> do
        let ops = [op | (pred, op) <- opts, pred lexeme]
        case ops of
          [] -> return left
          op : _ -> do
            f <- op
            right <- lookchainr1 p opts
            rest (f left right)

lookchainl1 :: Parser a -> [(L.Lexeme -> Bool, Parser (a -> a -> a))] -> Parser a
lookchainl1 p opts = do left <- p; rest left
  where
    rest left =
      look >>= \lexeme -> do
        let ops = [op | (pred, op) <- opts, pred lexeme]
        case ops of
          [] -> return left
          op : _ -> do
            f <- op
            right <- p
            rest (f left right)

lookchainl1' :: Parser a -> [(L.Lexeme -> Bool, Parser (a -> a))] -> Parser a
lookchainl1' p opts = do left <- p; rest left
  where
    rest left =
      look >>= \lexeme -> do
        let ops = [op | (pred, op) <- opts, pred lexeme]
        case ops of
          [] -> return left
          op : _ -> do
            f <- op
            rest (f left)
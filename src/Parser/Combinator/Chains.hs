module Parser.Combinator.Chains where

import Control.Applicative (Alternative ((<|>)))

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
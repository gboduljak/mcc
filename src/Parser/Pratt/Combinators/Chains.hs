module Parser.Pratt.Combinators.Chains where

import qualified Lexer.Lexeme as L
import Parser.Pratt.Prim (Parser, lookahead)

lookchainl1 :: Parser a -> (L.Lexeme -> Bool) -> Parser (a -> a) -> Parser a
lookchainl1 p pred op = do left <- p; rest left
  where
    rest left =
      lookahead >>= \lexeme -> do
        if pred lexeme
          then do
            f <- op
            rest (f left)
          else do return left
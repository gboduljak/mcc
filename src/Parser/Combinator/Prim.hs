module Parser.Combinator.Prim where

import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Void
import Lexer.Lexeme as L
import Lexer.Token as T
import qualified Parser.Ast as Ast
import Parser.Combinator.TokenStream
import Text.Megaparsec

type Parser = Parsec Void TokenStream

liftToken :: L.Lexeme -> T.Token
liftToken lexeme = T.Token lexeme pos pos 0
  where
    pos = initialPos ""

expect :: L.Lexeme -> Parser L.Lexeme
expect lex = token test (Set.singleton . Tokens . nes . liftToken $ lex)
  where
    test (Token lex' _ _ _) =
      if lex' == lex
        then Just lex
        else Nothing
    nes x = x :| []

look :: Parser L.Lexeme
look = do
  tok <- lookAhead (try anySingle)
  return (T.lexeme tok)
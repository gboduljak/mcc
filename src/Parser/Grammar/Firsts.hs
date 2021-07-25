module Parser.Grammar.Firsts where

import Lexer.Lexeme
import qualified Lexer.Lexeme as L

startsExpr :: Lexeme -> Bool
startsExpr lexeme =
  L.any [LParen, Minus, Not, Asterisk, Ampers, Sizeof, LitNull] lexeme
    || L.isLit lexeme
    || L.isIdent lexeme

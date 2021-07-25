module Parser.Grammar.Firsts where

import Lexer.Lexeme
import qualified Lexer.Lexeme as L

startsExpr :: Lexeme -> Bool
startsExpr lexeme =
  L.any [LParen, Minus, Not, Asterisk, Ampers, Sizeof, LitNull] lexeme
    || L.isLit lexeme
    || L.isIdent lexeme

startsStmt :: Lexeme -> Bool
startsStmt lexeme =
  L.any [While, For, If, Return, LBrace] lexeme
    || startsType lexeme
    || startsExpr lexeme

startsType :: Lexeme -> Bool
startsType lexeme = L.is Struct lexeme || L.isType lexeme
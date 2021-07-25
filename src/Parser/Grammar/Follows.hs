module Parser.Grammar.Follows where

import Lexer.Lexeme (Lexeme)
import qualified Lexer.Lexeme as L
import Parser.Grammar.Operators (isInfix)

followsExp :: Lexeme -> Bool
followsExp lexeme = isInfix lexeme || L.any [L.Semi, L.RParen, L.RBrack, L.Comma] lexeme

followsStatement :: Lexeme -> Bool
followsStatement lexeme =
  L.any
    [ L.Else,
      L.While,
      L.For,
      L.If,
      L.Return,
      L.LBrace,
      L.Struct,
      L.LParen,
      L.Minus,
      L.Not,
      L.Asterisk,
      L.Ampers,
      L.Sizeof,
      L.LitNull,
      L.RBrace
    ]
    lexeme
    || L.isType lexeme
    || L.isLit lexeme
    || L.isIdent lexeme

followsConstruct :: Lexeme -> Bool
followsConstruct lexeme = L.any [L.Struct, L.Eof] lexeme || L.isType lexeme

followsStructField :: Lexeme -> Bool
followsStructField = L.any [L.RBrace]
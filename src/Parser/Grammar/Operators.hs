module Parser.Grammar.Operators where

import qualified Lexer.Lexeme as L

isInfix :: L.Lexeme -> Bool
isInfix =
  L.any
    [ L.Assign,
      L.Or,
      L.And,
      L.Bar,
      L.Caret,
      L.Ampers,
      L.Equal,
      L.Neq,
      L.Less,
      L.Leq,
      L.Greater,
      L.Geq,
      L.Plus,
      L.Minus,
      L.Asterisk,
      L.Div,
      L.Mod,
      L.LBrack,
      L.Dot,
      L.Arrow
    ]

precedence :: L.Lexeme -> Int
precedence L.Assign = 10
precedence L.Or = 20
precedence L.And = 30
precedence L.Bar = 40
precedence L.Caret = 50
precedence L.Ampers = 60
precedence L.Equal = 70
precedence L.Neq = 70
precedence L.Less = 80
precedence L.Leq = 80
precedence L.Greater = 80
precedence L.Geq = 80
precedence L.Plus = 100
precedence L.Minus = 100
precedence L.Asterisk = 110
precedence L.Div = 110
precedence L.Mod = 110
precedence L.Dot = 130
precedence L.LBrack = 130
precedence L.Arrow = 130
precedence _ = 0

typecastPrecedence :: Int
typecastPrecedence = 120
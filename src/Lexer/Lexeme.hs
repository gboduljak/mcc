module Lexer.Lexeme where

data Lexeme
  = LitInt Int
  | LitDouble Double
  | LitString String
  | LitChar Char
  | LitNull
  | Ident String
  | Type BuiltinType
  | Struct
  | Return
  | Assign
  | Comma
  | Semi
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | For
  | While
  | If
  | Else
  | Plus
  | Minus
  | Asterisk
  | Div
  | Mod
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | Not
  | Ampers
  | Bar
  | Caret
  | Dot
  | Arrow
  | Sizeof
  deriving (Show)

data BuiltinType = Int | Double | Char | Void deriving (Show)
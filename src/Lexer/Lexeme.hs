{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Lexer.Lexeme where

import Data.Char (chr)
import Data.Text
import Data.Text.Prettyprint.Doc
import Data.Void

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
  | Eof
  | Include
  | Error
  deriving (Show, Eq)

data BuiltinType = Int | Double | Char | Void deriving (Show, Eq)

instance Pretty Lexeme where
  pretty = \case
    (LitInt x) -> pretty x
    (LitDouble x) -> pretty x
    (LitChar c) -> squotes $ pretty c
    (LitString s) -> dquotes $ pretty s
    LitNull -> pretty "NULL"
    (Ident x) -> pretty x
    (Type Int) -> pretty "int"
    (Type Double) -> pretty "double"
    (Type Char) -> pretty "char"
    (Type Void) -> pretty "void"
    Struct -> pretty "struct"
    Return -> pretty "return"
    Assign -> pretty "="
    Comma -> pretty ","
    Semi -> pretty ";"
    LParen -> pretty "("
    RParen -> pretty ")"
    LBrace -> pretty "{"
    RBrace -> pretty "}"
    LBrack -> pretty "["
    RBrack -> pretty "]"
    For -> pretty "for"
    While -> pretty "while"
    If -> pretty "if"
    Else -> pretty "else"
    Plus -> pretty "+"
    Minus -> pretty "-"
    Asterisk -> pretty "*"
    Div -> pretty "/"
    Mod -> pretty "%"
    Equal -> pretty "=="
    Neq -> pretty "!="
    Less -> pretty "<"
    Leq -> pretty "<="
    Greater -> pretty ">"
    Geq -> pretty ">="
    And -> pretty "&&"
    Or -> pretty "||"
    Not -> pretty "!"
    Ampers -> pretty "&"
    Bar -> pretty "|"
    Caret -> pretty "^"
    Dot -> pretty "."
    Arrow -> pretty "->"
    Sizeof -> pretty "sizeof"
    Include -> pretty "#include"
    Error -> pretty "error"
    Eof -> emptyDoc
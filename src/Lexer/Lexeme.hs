{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Lexer.Lexeme where

import Data.Char (chr)
import Data.Text hiding (any)
import Data.Text.Prettyprint.Doc
import Data.Void
import Prelude hiding (any)

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
  | Increment
  | Decrement
  | Error
  deriving (Show, Eq, Ord)

data BuiltinType = Int | Double | Char | Void deriving (Show, Eq, Ord)

isType :: Lexeme -> Bool
isType (Type _) = True
isType _ = False

isLitInt :: Lexeme -> Bool
isLitInt (LitInt _) = True
isLitInt _ = False

isLitDouble :: Lexeme -> Bool
isLitDouble (LitDouble _) = True
isLitDouble _ = False

isLitChar :: Lexeme -> Bool
isLitChar (LitChar _) = True
isLitChar _ = False

isLitString :: Lexeme -> Bool
isLitString (LitString _) = True
isLitString _ = False

isLit :: Lexeme -> Bool
isLit lexeme =
  or
    [ isLitInt lexeme,
      isLitDouble lexeme,
      isLitChar lexeme,
      isLitString lexeme
    ]

isIdent :: Lexeme -> Bool
isIdent (Ident _) = True
isIdent _ = False

any :: [Lexeme] -> Lexeme -> Bool
any opts = (`elem` opts)

is :: Lexeme -> Lexeme -> Bool
is lexeme = any [lexeme]

instance Pretty BuiltinType where
  pretty Int = pretty "int"
  pretty Double = pretty "double"
  pretty Char = pretty "char"
  pretty Void = pretty "void"

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
    Increment -> pretty "++"
    Decrement -> pretty "--"
    Eof -> emptyDoc

display :: Lexeme -> String
display = \case
  (LitInt x) -> "integer literal"
  (LitDouble x) -> "double literal"
  (LitChar c) -> "char literal"
  (LitString s) -> "string literal"
  LitNull -> "NULL"
  (Ident x) -> "identifier"
  (Type Int) -> "int"
  (Type Double) -> "double"
  (Type Char) -> "char"
  (Type Void) -> "void"
  Struct -> "struct"
  Return -> "return"
  Assign -> "\'=\'"
  Comma -> "\',\'"
  Semi -> "\';\'"
  LParen -> "\'(\'"
  RParen -> "\')\'"
  LBrace -> "\'{\'"
  RBrace -> "\'}\'"
  LBrack -> "\'[\'"
  RBrack -> "\']\'"
  For -> "for"
  While -> "while"
  If -> "if"
  Else -> "else"
  Plus -> "+"
  Minus -> "\'-\'"
  Asterisk -> "\'*\'"
  Div -> "\'/\'"
  Mod -> "\'%\'"
  Equal -> "\'==\'"
  Neq -> "\'!=\'"
  Less -> "\'<\'"
  Leq -> "\'<=\'"
  Greater -> "\'>\'"
  Geq -> "\'>=\'"
  And -> "\'&&\'"
  Or -> "\'||\'"
  Not -> "\'!\'"
  Ampers -> "\'&\'"
  Bar -> "\'|\'"
  Caret -> "\'^\'"
  Dot -> "\'.\'"
  Arrow -> "\'->\'"
  Sizeof -> "sizeof"
  Include -> "#include"
  Error -> "error"
  Increment -> "++"
  Decrement -> "--"
  Eof -> "EOF"

defaultLexemes :: [Lexeme]
defaultLexemes =
  [ LitInt 0,
    LitDouble 0,
    LitString "",
    LitChar ' ',
    LitNull,
    Ident "",
    Type Int,
    Type Double,
    Type Char,
    Type Void,
    Struct,
    Return,
    Assign,
    Comma,
    Semi,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    For,
    While,
    If,
    Else,
    Plus,
    Minus,
    Asterisk,
    Div,
    Mod,
    Equal,
    Neq,
    Less,
    Leq,
    Greater,
    Geq,
    And,
    Or,
    Not,
    Ampers,
    Bar,
    Caret,
    Dot,
    Arrow,
    Sizeof,
    Eof,
    Include,
    Increment,
    Decrement,
    Error
  ]
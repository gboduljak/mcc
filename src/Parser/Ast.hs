{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Parser.Ast where

import Data.Text.Prettyprint.Doc
  ( Pretty (pretty),
    concatWith,
    enclose,
    encloseSep,
    hardline,
    hcat,
    indent,
    lbrace,
    lparen,
    parens,
    rbrace,
    rparen,
    semi,
    tupled,
    vsep,
    (<+>),
  )
import Lexer.Lexeme (BuiltinType (Int))

data Program = Program [Directive] [Construct] deriving (Show)

data Directive = Include String deriving (Show)

data Construct
  = StructDecl StructDecl
  | FuncDecl FuncDecl
  | VarDecl VarDecl
  deriving (Show)

data StructDecl = Struct String [VarDecl] deriving (Show)

data FuncDecl = Func Type String [Formal] Block deriving (Show)

data VarDecl = Var Type String [Int] deriving (Show)

data Formal = Formal Type String deriving (Show)

data Block = Block [Statement] deriving (Show)

data Statement
  = Expr Expr
  | BlockStatement Block
  | VarDeclStatement VarDecl
  | While Expr Statement
  | For (Maybe Expr) (Maybe Expr) (Maybe Expr) Statement
  | If Expr Statement (Maybe Statement)
  | Return (Maybe Expr)
  deriving (Show)

data Expr
  = LitInt Int
  | LitDouble Double
  | LitString String
  | LitChar Char
  | Null
  | Ident String
  | Nested Expr
  | Binop Expr InfixOp Expr
  | Deref Expr
  | AddressOf Expr
  | Negate Expr
  | Negative Expr
  | FieldAccess Expr String
  | ArrayAccess Expr Expr
  | Indirect Expr String
  | Sizeof (Either Type Expr)
  | Typecast Type Expr
  | Call String [Expr]
  | Assign Expr Expr
  deriving (Show)

data Type
  = PrimitiveType BuiltinType Int
  | StructType String Int
  deriving (Show)

data InfixOp
  = Add
  | Sub
  | Mul
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
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  deriving (Show)

isPointer :: Type -> Bool
isPointer (PrimitiveType _ ptrs) = ptrs > 0
isPointer (StructType _ ptrs) = ptrs > 0

isPrimitive :: Type -> Bool
isPrimitive (PrimitiveType _ _) = True
isPrimitive (StructType _ _) = False

isStruct :: Type -> Bool
isStruct = not . isPrimitive

isArray :: VarDecl -> Bool
isArray (Var _ _ x) = (not . null) x
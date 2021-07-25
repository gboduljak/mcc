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

data Program = Program [Directive] [Construct] deriving (Show, Eq)

data Directive = Include String deriving (Show, Eq)

data Construct
  = StructDecl StructDecl
  | FuncDecl FuncDecl
  | FuncDefn FuncDef
  | VarDecl VarDecl
  | ConstructError
  deriving (Show, Eq)

data StructDecl = Struct String [VarDecl] deriving (Show, Eq)

data FuncDecl = Func Type String [Formal] deriving (Show, Eq)

data FuncDef = FuncDef Type String [Formal] Block deriving (Show, Eq)

data VarDecl = Var Type String [Int] | VarDeclError deriving (Show, Eq)

data Formal = Formal Type String | FormalError deriving (Show, Eq)

data Block = Block [Statement] deriving (Show, Eq)

data Statement
  = Expr Expr
  | BlockStatement Block
  | VarDeclStatement VarDecl
  | While Expr Statement
  | For (Maybe Expr) (Maybe Expr) (Maybe Expr) Statement
  | If Expr Statement (Maybe Statement)
  | Return (Maybe Expr)
  | StatementError
  deriving (Show, Eq)

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
  | ExprError
  deriving (Show, Eq)

data Type
  = PrimitiveType BuiltinType Int
  | StructType String Int
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
isArray _ = False
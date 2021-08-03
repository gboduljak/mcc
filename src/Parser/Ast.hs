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

data StructDecl = Struct String [VarDecl] Int deriving (Show, Eq)

data FuncDecl = Func Type String [Formal] Int deriving (Show, Eq)

data FuncDef = FuncDef Type String [Formal] Block Int deriving (Show, Eq)

data VarDecl = Var Type String [Int] Int | VarDeclError Int deriving (Show, Eq)

data Formal = Formal Type String Int | FormalError Int deriving (Show, Eq)

data Block = Block [Statement] Int deriving (Show, Eq)

data Statement
  = Expr Expr Int
  | BlockStatement Block Int
  | VarDeclStatement VarDecl Int
  | While Expr Statement Int
  | For (Maybe Expr) (Maybe Expr) (Maybe Expr) Statement Int
  | If Expr Statement (Maybe Statement) Int
  | Return (Maybe Expr) Int
  | StatementError Int
  deriving (Show, Eq)

data Expr
  = LitInt Int Int
  | LitDouble Double Int
  | LitString String Int
  | LitChar Char Int
  | Null Int
  | Ident String Int
  | Nested Expr Int
  | Binop Expr InfixOp Expr Int
  | Deref Expr Int
  | AddressOf Expr Int
  | Negate Expr Int
  | Negative Expr Int
  | FieldAccess Expr String Int
  | ArrayAccess Expr Expr Int
  | Indirect Expr String Int
  | Sizeof (Either Type Expr) Int
  | Typecast Type Expr Int
  | Call String [Expr] Int
  | Assign Expr Expr Int
  | ExprError Int
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

pointerLevel :: Type -> Int
pointerLevel (PrimitiveType _ ptrs) = ptrs
pointerLevel (StructType _ ptrs) = ptrs

decreasePointerLevel :: Type -> Int -> Type
decreasePointerLevel (PrimitiveType typ ptrs) n = PrimitiveType typ (ptrs - n)
decreasePointerLevel (StructType name ptrs) n = StructType name (ptrs - n)

isStruct :: Type -> Bool
isStruct = not . isPrimitive

isArray :: VarDecl -> Bool
isArray (Var _ _ x _) = (not . null) x
isArray _ = False

-- data Expr
--   = LitInt Int Int
--   | LitDouble Double Int
--   | LitString String Int
--   | LitChar Char Int
--   | Null Int
--   | Ident String Int
--   | Nested Expr Int
--   | Binop Expr InfixOp Expr Int
--   | Deref Expr Int
--   | AddressOf Expr Int
--   | Negate Expr Int
--   | Negative Expr Int
--   | FieldAccess Expr String Int
--   | ArrayAccess Expr Expr Int
--   | Indirect Expr String Int
--   | Sizeof (Either Type Expr) Int
--   | Typecast Type Expr Int
--   | Call String [Expr] Int
--   | Assign Expr Expr Int
--   | ExprError Int
getExprOff :: Expr -> Int
getExprOff (LitInt _ off) = off
getExprOff (LitDouble _ off) = off
getExprOff (LitString _ off) = off
getExprOff (LitChar _ off) = off
getExprOff (Null off) = off
getExprOff (Ident _ off) = off
getExprOff (Nested _ off) = off
getExprOff (Binop _ _ _ off) = off
getExprOff (Deref _ off) = off
getExprOff (AddressOf _ off) = off
getExprOff (Negate _ off) = off
getExprOff (Negative _ off) = off
getExprOff (FieldAccess _ _ off) = off
getExprOff (ArrayAccess _ _ off) = off
getExprOff (Indirect _ _ off) = off
getExprOff (Sizeof _ off) = off
getExprOff (Typecast _ _ off) = off
getExprOff (Call _ _ off) = off
getExprOff (Assign _ _ off) = off
getExprOff (ExprError off) = off

getBlockOffset :: Block -> Int
getBlockOffset (Block _ off) = off
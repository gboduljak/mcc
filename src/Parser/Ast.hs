module Parser.Ast where

import Lexer.Lexeme (BuiltinType, Lexeme (Assign, LitInt, Return, Sizeof))

data Program = Program [Construct] deriving (Show)

data Construct
  = StructDecl StructDecl
  | FuncDecl FuncDecl
  | VarDecl VarDecl
  deriving (Show)

data StructDecl = Struct String [VarDecl] deriving (Show)

data FuncDecl = Func Type String [Formal] Block deriving (Show)

data VarDecl = Var Type String [Int] deriving (Show)

data Formal = Formal Type Int String deriving (Show)

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
  | Unop PrefixOp Expr
  | Deref Expr
  | AddressOf Expr
  | Negate Expr
  | Negative Expr
  | FieldAccess Expr String
  | ArrayAccess Expr Expr
  | Indirect Expr String
  | Sizeof Type
  | Typecast Type [Int] Expr
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

data PrefixOp
  = UnaryMinus
  | Not
  deriving (Show)
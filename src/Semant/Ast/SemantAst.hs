module Semant.Ast.SemantAst where

import qualified Data.Map as Map
import Parser.Ast (InfixOp)
import qualified Parser.Ast as Ast
import Semant.Type (Type)

data SProgram = SProgram
  { structs :: [SStruct],
    funcs :: [SFunction],
    globals :: [SVarDecl]
  }

data SStruct = SStruct
  { structName :: String,
    structFields :: [SVarDecl],
    structFieldOffsets :: Map.Map String Int
  }

data SFunction = SFunction
  { funcRetType :: Type,
    funcName :: String,
    formals :: [SFormal],
    body :: Maybe SBlock
  }

data SFormal = SFormal Type String deriving (Show, Eq)

data SBlock = SBlock [SStatement] deriving (Show, Eq)

data SVarDecl = SVar Type String [Int] deriving (Show, Eq)

data SStatement
  = SExpr SExpr
  | SBlockStatement SBlock
  | SVarDeclStatement SVarDecl
  | SWhile SExpr SStatement
  | SFor (Maybe SExpr) (Maybe SExpr) (Maybe SExpr) SStatement
  | SIf SExpr SStatement (Maybe SStatement)
  | SReturn (Maybe SExpr)
  deriving (Show, Eq)

type SExpr = (Type, SExpr')

data SExpr'
  = SLitInt Int
  | SLitDouble Double
  | SLitString String
  | SLitChar Char
  | SNull
  | SNoExpr
  | SBinop SExpr InfixOp SExpr
  | SAddr SExpr
  | SNegate SExpr
  | SNegative SExpr
  | SSizeof (Either Ast.Type SExpr)
  | STypecast Type SExpr
  | LVal LValue
  | SAssign LValue SExpr
  deriving (Show, Eq)

data LValue
  = SDeref SExpr
  | SIdent String
  | SFieldAccess SExpr String
  | SArrayAccess SExpr [SExpr]
  | SIndirect SExpr String
  deriving (Show, Eq)
{-# LANGUAGE RecordWildCards #-}

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
  deriving (Show, Eq)

data SStruct = SStruct
  { structName :: String,
    fields :: [SVarDecl],
    fieldOffsets :: Map.Map String Int
  }
  deriving (Show, Eq)

data SFunction = SFunction
  { returnType :: Type,
    funcName :: String,
    formals :: [SFormal],
    body :: Maybe SBlock
  }
  deriving (Show, Eq)

data SFormal = SFormal Type String deriving (Show, Eq)

data SBlock = SBlock [SStatement] deriving (Show, Eq)

data SVarDecl = SVar Type String deriving (Show, Eq)

data SStatement
  = SExpr SExpr
  | SBlockStatement SBlock
  | SVarDeclStatement SVarDecl
  | SDoWhile SExpr SStatement
  | SIf SExpr SStatement (Maybe SStatement)
  | SReturn SExpr
  deriving (Show, Eq)

type SExpr = (Type, SExpr')

data SExpr'
  = SLitInt Int
  | SLitDouble Double
  | SLitString String
  | SLitChar Char
  | SNull
  | SEmptyExpr
  | SBinop SExpr InfixOp SExpr
  | SAddressOf LValue
  | SNegate SExpr
  | SNegative SExpr
  | SSizeof (Either Ast.SizeofType SExpr)
  | STypecast Ast.Type SExpr
  | SCall String [SExpr]
  | LVal LValue
  | SAssign SExpr SExpr
  deriving (Show, Eq)

data LValue
  = SDeref SExpr
  | SIdent String
  | SFieldAccess SExpr String
  | SArrayAccess SExpr [SExpr]
  | SNoAddrLVal
  deriving (Show, Eq)

getFields :: String -> SStruct -> [SVarDecl]
getFields name SStruct {..} = [field | field@(SVar fieldType fieldName) <- fields, fieldName == name]

getFieldOffset :: String -> SStruct -> Maybe Int
getFieldOffset fieldName SStruct {..} = Map.lookup fieldName fieldOffsets

varName :: SVarDecl -> String
varName (SVar _ name) = name

isLValue :: SExpr' -> Bool
isLValue (LVal _) = True
isLValue _ = False
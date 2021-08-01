module Semant.Type where

import Data.Text.Prettyprint.Doc
import Lexer.Lexeme (BuiltinType (Char, Double, Int, Void))
import qualified Parser.Ast as Ast
import Parser.AstPrettyPrinter

data Type = Scalar Ast.Type | Array Ast.Type [Int] | Any deriving (Show, Eq)

numericTypes :: [Type]
numericTypes =
  [ Scalar (Ast.PrimitiveType Int 0),
    Scalar (Ast.PrimitiveType Double 0),
    Scalar (Ast.PrimitiveType Char 0)
  ]

instance Pretty Type where
  pretty (Scalar typ) = pretty typ
  pretty (Array typ sizes) = pretty typ <+> pretty sizes
  pretty Any = pretty "any"

isCond :: Type -> Bool
isCond condTyp =
  condTyp == Any
    || isInt condTyp
    || isDouble condTyp
    || isChar condTyp
    || isPointer condTyp

voidTyp :: Type
voidTyp = Scalar (Ast.PrimitiveType Void 0)

isInt :: Type -> Bool
isInt (Scalar (Ast.PrimitiveType Int 0)) = True
isInt _ = False

isDouble :: Type -> Bool
isDouble (Scalar (Ast.PrimitiveType Double 0)) = True
isDouble _ = False

isChar :: Type -> Bool
isChar (Scalar (Ast.PrimitiveType Char 0)) = True
isChar _ = False

isNumeric :: Type -> Bool
isNumeric typ = typ `elem` numericTypes

isPointer :: Type -> Bool
isPointer (Scalar (Ast.PrimitiveType Void ptrs)) = ptrs > 0
isPointer typ = isNonVoidPointer typ

isArray :: Type -> Bool
isArray (Array _ _) = True
isArray _ = False

isNonVoidPointer :: Type -> Bool
isNonVoidPointer (Scalar (Ast.PrimitiveType _ ptrs)) = ptrs > 0
isNonVoidPointer (Scalar (Ast.StructType _ ptrs)) = ptrs > 0
isNonVoidPointer Any = True
isNonVoidPointer _ = False

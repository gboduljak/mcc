{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Codegen.TypeMappings where

import LLVM.AST (Operand, Type (StructureType))
import qualified Semant.Type as Semant
import qualified LLVM.AST.Type as LLVM.AST
import Parser.Ast (Type(PrimitiveType, StructType))
import Lexer.Lexeme (BuiltinType(..))
import Semant.Type (Type(..))
import Control.Monad.State (MonadState (get))
import Codegen.Env (Env)
import Codegen.Codegen (Codegen, LLVM, lookupStruct)
import LLVM.Prelude (foldlM)
import Data.Foldable (foldlM)
import Codegen.Signatures.StructSignature
import LLVM.AST.Name (mkName)

charPtr :: LLVM.AST.Type
charPtr = LLVM.AST.ptr LLVM.AST.i8

llvmType :: (MonadState (Env Operand) m) => Semant.Type -> m LLVM.AST.Type
llvmType (Scalar (PrimitiveType Void 0)) = return LLVM.AST.void
llvmType (Scalar (PrimitiveType Int 0)) = return LLVM.AST.i32
llvmType (Scalar (PrimitiveType Char 0)) = return LLVM.AST.i8
llvmType (Scalar (PrimitiveType Double 0)) = return LLVM.AST.double
llvmType (Scalar (PrimitiveType Void 1)) = return charPtr
llvmType (Scalar (PrimitiveType typ ptrs)) = do
  innerTyp <- llvmType (Scalar (PrimitiveType typ (ptrs - 1)))
  return (LLVM.AST.ptr innerTyp)
llvmType (Scalar (StructType name 0)) = return $ LLVM.AST.NamedTypeReference (llvmStructName name)
llvmType (Scalar (StructType name ptrs)) = do
  innerTyp <- llvmType (Scalar (StructType name (ptrs - 1)))
  return (LLVM.AST.ptr innerTyp)
llvmType (Array primTyp []) = llvmType (Scalar primTyp)
llvmType (Array primTyp sizes) = do
  baseTyp <- llvmType (Scalar primTyp)
  let arrayTyp = foldr (LLVM.AST.ArrayType . fromIntegral) baseTyp sizes
  return arrayTyp
llvmType Any = error "semantic analysis failed"

llvmSizeOf :: (MonadState (Env Operand) m) => Semant.Type.Type -> m Int
llvmSizeOf (Scalar (PrimitiveType Char 0)) = return 1
llvmSizeOf (Scalar (PrimitiveType Int 0)) = return 4
llvmSizeOf (Scalar (PrimitiveType Double 0)) = return 8
llvmSizeOf (Scalar (PrimitiveType Void 0)) = return 0
llvmSizeOf (Scalar (PrimitiveType _ ptrs)) = return 8
llvmSizeOf (Scalar (StructType name 0)) = do
  structFields <- fields <$> lookupStruct name
  structFieldSizes <- mapM llvmSizeOf (semantFieldType <$> structFields)
  return (sum structFieldSizes)
llvmSizeOf (Scalar (StructType _ ptrs)) = return 8
llvmSizeOf (Array baseType arraySizes) = do
  baseTypSize <- llvmSizeOf (Scalar baseType)
  return (baseTypSize * product arraySizes)
llvmSizeOf Any = error "semantic analysis failed"

llvmStructType :: StructSignature -> LLVM.AST.Type
llvmStructType StructSignature{..} = StructureType {
  isPacked = True,
  elementTypes = llvmFieldType <$> fields
}
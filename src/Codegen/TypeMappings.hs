{-# LANGUAGE FlexibleContexts #-}
module Codegen.TypeMappings where

import LLVM.AST (Operand, Type)
import qualified Semant.Type as Semant
import qualified LLVM.AST.Type as LLVM.AST
import Parser.Ast (Type(PrimitiveType, StructType))
import Lexer.Lexeme (BuiltinType(..))
import Semant.Type (Type(Scalar, Array))
import Control.Monad.State (MonadState (get))
import Codegen.Env (Env)
import Codegen.Codegen (Codegen, LLVM)
import LLVM.Prelude (foldlM)
import Data.Foldable (foldlM)

charPtr :: LLVM.AST.Type
charPtr = LLVM.AST.ptr LLVM.AST.i8

llvmType :: (MonadState (Env a) m) => Semant.Type -> m LLVM.AST.Type
llvmType (Scalar (PrimitiveType Void 0)) = return LLVM.AST.void
llvmType (Scalar (PrimitiveType Int 0)) = return LLVM.AST.i32
llvmType (Scalar (PrimitiveType Char 0)) = return LLVM.AST.i8
llvmType (Scalar (PrimitiveType Double 0)) = return LLVM.AST.double
llvmType (Scalar (PrimitiveType Void 1)) = return charPtr
llvmType (Scalar (PrimitiveType typ ptrs)) = do
  innerTyp <- llvmType (Scalar (PrimitiveType typ (ptrs - 1)))
  return (LLVM.AST.ptr innerTyp)
llvmType (Array primTyp []) = llvmType (Scalar primTyp)
llvmType (Array primTyp sizes) = do
  baseTyp <- llvmType (Scalar primTyp)
  let arrayTyp = foldr (LLVM.AST.ArrayType . fromIntegral) baseTyp sizes
  return arrayTyp
llvmType _ = undefined

llvmSizeOf :: (MonadState (Env a) m) => Semant.Type.Type -> m Int
llvmSizeOf (Scalar (PrimitiveType Char 0)) = return 1
llvmSizeOf (Scalar (PrimitiveType Int 0)) = return 4
llvmSizeOf (Scalar (PrimitiveType Double 0)) = return 8
llvmSizeOf (Scalar (PrimitiveType Void 0)) = return 0
llvmSizeOf (Scalar (PrimitiveType _ ptrs)) = return 8
llvmSizeOf (Array baseType arraySizes) = do
  baseTypSize <- llvmSizeOf (Scalar baseType)
  return (baseTypSize * product arraySizes)
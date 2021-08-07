{-# LANGUAGE FlexibleContexts #-}
module Codegen.TypeMappings where 

import LLVM.AST (Operand, Type)
import qualified Semant.Type as Semant
import qualified LLVM.AST.Type as LLVM.AST
import Parser.Ast (Type(PrimitiveType, StructType))
import Lexer.Lexeme (BuiltinType(..))
import Semant.Type (Type(Scalar))
import Control.Monad.State (MonadState (get))
import Codegen.Env (Env)
import Codegen.Codegen (Codegen, LLVM)

charPtr :: LLVM.AST.Type
charPtr = LLVM.AST.ptr LLVM.AST.i8


llvmType :: Semant.Type -> LLVM LLVM.AST.Type
llvmType (Scalar (PrimitiveType Void 0)) = return LLVM.AST.void
llvmType (Scalar (PrimitiveType Int 0)) = return LLVM.AST.i32
llvmType (Scalar (PrimitiveType Char 0)) = return LLVM.AST.i8
llvmType (Scalar (PrimitiveType Double 0)) = return LLVM.AST.double
llvmType (Scalar (PrimitiveType Void 1)) = return charPtr
llvmType (Scalar (PrimitiveType typ ptrs)) = do 
  innerTyp <- llvmType (Scalar (PrimitiveType typ (ptrs - 1)))
  return (LLVM.AST.ptr innerTyp)
llvmType _ = undefined
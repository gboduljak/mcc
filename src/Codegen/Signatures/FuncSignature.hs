{-# LANGUAGE FlexibleContexts #-}
module Codegen.Signatures.FuncSignature where 

import qualified LLVM.AST
import Semant.Ast.SemantAst
import Control.Monad.State (MonadState)
import Codegen.Env (Env)
import LLVM.AST (Type(FunctionType))
import Codegen.TypeMappings (llvmType)
import qualified Semant.Type
import Semant.Type (Type(Scalar))
import Parser.Ast (Type(StructType))

data FuncSignature = FuncSignature {
  funcName :: String,
  funcRetTyp :: LLVM.AST.Type,
  funcParams :: [(LLVM.AST.Type, String)],
  funcType :: LLVM.AST.Type
}
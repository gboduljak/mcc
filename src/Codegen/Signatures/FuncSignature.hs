{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Codegen.Signatures.FuncSignature where 

import qualified LLVM.AST
import Semant.Ast.SemantAst
import Control.Monad.State (MonadState)
import Codegen.Env (Env)
import LLVM.AST (Type(FunctionType), Operand (ConstantOperand), Name)
import Codegen.TypeMappings (llvmType)
import qualified Semant.Type
import Semant.Type (Type(Scalar))
import Parser.Ast (Type(StructType))
import LLVM.AST.Constant

data FuncSignature = FuncSignature {
  funcName :: String,
  funcRetTyp :: LLVM.AST.Type,
  funcParams :: [(LLVM.AST.Type, String)],
  funcType :: LLVM.AST.Type
}

llvmFuncSignature :: MonadState (Env Operand) m => SFunction -> m FuncSignature
llvmFuncSignature SFunction{..} = do 
  funcRetTyp <- llvmType returnType
  funcParams <- mapM (\(SFormal paramTyp paramName) -> do 
      paramLlvmTyp <- llvmParamType paramTyp 
      return (paramLlvmTyp, paramName)
    ) formals
  let funcTyp = FunctionType {
      resultType = funcRetTyp,
      argumentTypes = fst <$> funcParams,
      isVarArg = False
    }
  return (FuncSignature funcName funcRetTyp funcParams funcTyp)

llvmParamType :: MonadState (Env Operand) m => Semant.Type.Type -> m LLVM.AST.Type
llvmParamType (Scalar (StructType name 0)) = return undefined -- implement as a byval struct pointer
llvmParamType typ =  llvmType typ

llvmFuncOperand :: (Monad m) => LLVM.AST.Type -> Name -> m Operand
llvmFuncOperand funcTyp funcName = return (ConstantOperand $ GlobalReference funcTyp funcName)
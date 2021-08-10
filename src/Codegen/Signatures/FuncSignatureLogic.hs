{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Codegen.Signatures.FuncSignatureLogic 

where 


import Control.Monad.State (MonadState)
import Semant.Type
import Parser.Ast (Type(StructType))
import LLVM.AST (Operand (ConstantOperand), Type, Name)
import Codegen.Env
import Semant.Ast.SemantAst
import Codegen.Signatures.FuncSignature
import LLVM.AST.Type (Type(..), ptr)
import Codegen.TypeMappings (llvmType)
import LLVM.AST.Constant
import Semant.Type (isStruct)

llvmFuncSignature :: MonadState (Env Operand) m => SFunction -> m FuncSignature
llvmFuncSignature SFunction{..} = do 
  funcRetTyp <- llvmParamType returnType
  funcParams <- mapM (\(SFormal paramTyp paramName) -> do 
      paramLlvmTyp <- llvmParamType paramTyp 
      return (paramTyp, paramLlvmTyp, paramName)
    ) formals
  let funcTyp = FunctionType {
      resultType = funcRetTyp,
      argumentTypes = llvmType <$> funcParams,
      isVarArg = False
    }
  return (
    FuncSignature 
      funcName 
      funcRetTyp 
      returnType 
      funcParams 
      funcTyp 
      (isStruct returnType)
    )
  where 
    llvmType (_, typ, _) = typ


llvmParamType :: MonadState (Env Operand) m => Semant.Type.Type -> m LLVM.AST.Type
-- dirty hack for passing structs by value and returning structs by value
llvmParamType typ@(Scalar (StructType name 0)) = do 
  structTyp <- llvmType typ 
  return (LLVM.AST.Type.ptr structTyp)
llvmParamType typ =  llvmType typ

llvmFuncOperand :: (Monad m) => LLVM.AST.Type -> Name -> m Operand
llvmFuncOperand funcTyp funcName = return (ConstantOperand $ GlobalReference funcTyp funcName)
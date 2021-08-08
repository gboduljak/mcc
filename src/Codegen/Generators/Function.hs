{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Codegen.Generators.Function

where

import Semant.Ast.SemantAst (SFunction (..), SFormal (SFormal), SBlock (SBlock))
import Codegen.Generators.Common (generateTerm)
import qualified Semant.Type (isVoid, Type)
import Codegen.TypeMappings (llvmType)
import LLVM.AST.Constant (Constant(GlobalReference))
import LLVM.AST (Type(..), mkName, Operand (ConstantOperand, LocalReference))
import Data.Maybe (isJust, fromJust)
import Codegen.Generators.Statement (generateBlock)
import qualified LLVM.IRBuilder as L
import Data.String.Conversions (cs)
import Data.String (fromString)
import Semant.Builtins (isBuiltin)
import LLVM.IRBuilder (ParameterName(ParameterName))
import SymbolTable.SymbolTable (enterScope, exitScope)
import LLVM.AST.Name (Name)
import qualified GHC.Base as AST
import qualified LLVM.AST.Type
import Semant.Type (Type(Scalar))
import Parser.Ast (Type(StructType))
import Codegen.Env (registerOperand, Env (funcs))
import Codegen.Codegen (registerFunc, LLVM, Codegen)
import Control.Monad.State (get, gets, MonadTrans (lift))
import Control.Monad
import qualified Data.Map as Map


generateFunctionDecl :: SFunction -> LLVM ()
generateFunctionDecl func@SFunction{..} =  do
  funcSign <- llvmFuncSignature func
  tempFuncOperand <- llvmFuncOperand (funcType funcSign) (mkName funcName)
  registerFunc funcName tempFuncOperand

generateFunctionDefn :: SFunction -> LLVM ()
generateFunctionDefn func@SFunction{..}
  | (not . hasBody) body = return ()
  | isBuiltin func = generateBuiltin func
  | otherwise = do
      funcSign <- llvmFuncSignature func
      actualFuncOperand <- L.function
        (mkName funcName)
        [(paramTyp, ParameterName (cs paramName)) | (paramTyp, paramName) <- funcParams funcSign]
        (funcRetTyp funcSign) (generateBody (funcRetTyp funcSign) funcBody (funcParams funcSign))
      registerFunc funcName actualFuncOperand

  where
    funcBody = extractBody body
    hasBody = isJust
    extractBody = fromJust

data FuncSignature = FuncSignature {
  funcName :: String,
  funcRetTyp :: LLVM.AST.Type,
  funcParams :: [(LLVM.AST.Type, String)],
  funcType :: LLVM.AST.Type
}

llvmFuncSignature :: SFunction -> LLVM FuncSignature
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

llvmParamType :: Semant.Type.Type -> LLVM LLVM.AST.Type
llvmParamType (Scalar (StructType name 0)) = return undefined -- implement as a byval struct pointer
llvmParamType typ =  llvmType typ

llvmFuncOperand :: LLVM.AST.Type -> Name -> LLVM Operand
llvmFuncOperand funcTyp funcName = return (ConstantOperand $ GlobalReference funcTyp funcName)

generateBody :: LLVM.AST.Type.Type -> SBlock -> [(LLVM.AST.Type.Type, String)] -> [Operand] -> Codegen ()
generateBody retTyp  body opMeta ops = do
  L.block `L.named` cs "entry"
  enterScope
  retValPtr <- L.alloca retTyp Nothing 0
  mapM_ (\((typ, name), op) -> do
    addr <- L.alloca typ Nothing 0
    L.store addr 0 op
    registerOperand name addr
    ) (zip opMeta ops)
  generateBlock body
  exitScope
  if isVoid retTyp 
    then generateTerm L.retVoid
    else do 
      generateTerm (
        do
          retVal <- L.load retValPtr 0
          L.ret retVal
        )
  where isVoid retTyp = retTyp == LLVM.AST.Type.void

generateBuiltin :: SFunction -> LLVM ()
generateBuiltin func@SFunction{..} = do 
  funcSign <- llvmFuncSignature func
  let retTyp = funcRetTyp funcSign
      paramTyps = [ paramTyp | (paramTyp, _) <- funcParams funcSign]
  funcOperand <- generateExtern funcName paramTyps retTyp
  registerFunc funcName funcOperand

generateExtern :: String -> [LLVM.AST.Type] -> LLVM.AST.Type -> LLVM Operand
generateExtern funcName paramTyps retTyp
  | funcName == "printf" = L.externVarArgs (mkName funcName) paramTyps retTyp
  | funcName == "scanf" = L.externVarArgs (mkName funcName) paramTyps retTyp
  | otherwise = L.extern (mkName funcName) paramTyps retTyp
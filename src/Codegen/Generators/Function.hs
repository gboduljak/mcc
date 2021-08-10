{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module Codegen.Generators.Function

where

import Semant.Ast.SemantAst (SFunction (..), SFormal (SFormal), SBlock (SBlock))
import Codegen.Generators.Common (generateTerm)
import Codegen.TypeMappings (llvmType, llvmSizeOf)
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
import Semant.Type ( Type(Scalar), isStruct )
import Parser.Ast (Type(StructType))
import Codegen.Env (registerOperand, Env (funcs))
import Codegen.Codegen (registerFunc, LLVM, Codegen)
import Control.Monad.State (get, gets, MonadTrans (lift), MonadState)
import qualified Data.Map as Map
import Codegen.Signatures.FuncSignature (FuncSignature(..))
import Codegen.Signatures.FuncSignatureLogic
    ( llvmFuncSignature, llvmFuncOperand )
import Codegen.Intrinsics.Memcpy (performMemcpy)

generateFunctionDecl :: SFunction -> LLVM ()
generateFunctionDecl func@SFunction{..} =  do
  funcSign <- llvmFuncSignature func
  tempFuncOperand <- llvmFuncOperand (funcType funcSign) (mkName funcName)
  registerFunc funcName funcSign tempFuncOperand

generateFunctionDefn :: SFunction -> LLVM ()
generateFunctionDefn func@SFunction{..}
  | (not . hasBody) body = return ()
  | isBuiltin func = generateBuiltin func
  | otherwise = do
      funcSign <- llvmFuncSignature func
      let funcName' = mkName funcName
      actualFuncOperand <- L.function
        funcName'
        [(paramLlvmTyp, ParameterName (cs paramName)) | (_, paramLlvmTyp, paramName) <- funcParams funcSign]
        (funcLLVMRetTyp funcSign) 
        (generateBody (funcLLVMRetTyp funcSign) funcBody (funcParams funcSign))
      registerFunc funcName funcSign actualFuncOperand
  where
    funcBody = extractBody body
    hasBody = isJust
    extractBody = fromJust

generateBody :: LLVM.AST.Type.Type -> SBlock -> [(Semant.Type.Type, LLVM.AST.Type, String)] -> [Operand] -> Codegen ()
generateBody retTyp body formalsMeta actuals = do
  L.block `L.named` cs "entry"
  enterScope
  mapM_ bindActualToFormal (zip formalsMeta actuals)
  generateBlock body
  exitScope
  if isVoid retTyp
    then generateTerm L.retVoid
    else do
      generateTerm (
        do
          retValPtr <- L.alloca retTyp Nothing 0
          retVal <- L.load retValPtr 0
          L.ret retVal
        )
  where isVoid retTyp = retTyp == LLVM.AST.Type.void

bindActualToFormal :: ((Semant.Type.Type, LLVM.AST.Type, String), Operand)  -> Codegen ()
bindActualToFormal ((semantTyp, llvmTyp, name), actual)
  -- bypass to accept structs 'by value'
  | isStruct semantTyp = do
      -- allocate space for struct on callee stack
      structLLVMTyp <- llvmType semantTyp
      structCopyPtr <- L.alloca structLLVMTyp Nothing 0
      structSize <- fromIntegral <$> llvmSizeOf semantTyp
      -- copy struct from caller stack to the callee stack
      performMemcpy structCopyPtr actual (L.int64 structSize)
      -- register copy
      registerOperand name structCopyPtr 
  -- all other formals work 'as expected'
  | otherwise = do
      addr <- L.alloca llvmTyp Nothing 0
      L.store addr 0 actual
      registerOperand name addr

generateBuiltin :: SFunction -> LLVM ()
generateBuiltin func@SFunction{..} = do
  funcSign <- llvmFuncSignature func
  let retTyp = funcLLVMRetTyp funcSign
      paramTyps = [ paramTyp | (_, paramTyp, _) <- funcParams funcSign]
  funcOperand <- generateExtern funcName paramTyps retTyp
  registerFunc funcName funcSign funcOperand

generateExtern :: String -> [LLVM.AST.Type] -> LLVM.AST.Type -> LLVM Operand
generateExtern funcName paramTyps retTyp
  | funcName == "printf" = L.externVarArgs (mkName funcName) paramTyps retTyp
  | funcName == "scanf" = L.externVarArgs (mkName funcName) paramTyps retTyp
  | otherwise = L.extern (mkName funcName) paramTyps retTyp
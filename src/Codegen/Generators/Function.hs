{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Codegen.Generators.Function

where

import Semant.Ast.SemantAst (SFunction (..), SFormal (SFormal), SBlock (SBlock))
import Codegen.Codegen
import Semant.Type (isVoid)
import Codegen.Env (registerOperand)
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

generateFunction :: SFunction -> LLVM ()
generateFunction func@SFunction{..}
  | (not . hasBody) body = return ()
  | isBuiltin func = return ()
  | isVoid returnType = do
      registerOperand funcName functionOperand
      x <- L.function
        functionName
        functionParameters
        functionReturnType (generateBody functionBody bodyParameters)
      return ()
  | otherwise = return ()

  where
    functionName = mkName funcName
    functionOperand = ConstantOperand $ GlobalReference functionType functionName
    functionReturnType = llvmType returnType
    functionBody = extractBody body
    functionParameters = [ (llvmType typ, ParameterName (cs name)) | SFormal typ name <- formals ]
    bodyParameters = [(name, llvmType typ) | SFormal typ name <- formals ]
    functionType = FunctionType {
        resultType = llvmType returnType,
        argumentTypes = map fst functionParameters,
        isVarArg = False
      }

    hasBody = isJust
    extractBody = fromJust

generateBody :: SBlock -> [(String, Type)] -> [Operand] -> Codegen ()
generateBody body opMeta ops = do
    L.block `L.named` cs "entry"
    enterScope
    mapM_ (\((name, typ), op) -> do
      addr <- L.alloca typ Nothing 0
      L.store addr 0 op
      registerOperand name op
      ) (zip opMeta ops)
    generateBlock body
    exitScope
    L.retVoid

-- FunctionType	
-- http://llvm.org/docs/LangRef.html#function-type

-- resultType :: Type	 
-- argumentTypes :: [Type]	 
-- isVarArg :: Bool

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Codegen.Generators.Structs

where

import qualified Data.Map as Map
import qualified LLVM.AST
import Semant.Ast.SemantAst (SStruct(..), SVarDecl (SVar), getFieldOffset)
import Codegen.Codegen (LLVM, registerStruct)
import Control.Monad.State (MonadState, modify)
import Codegen.Env
import Codegen.Signatures.StructSignature hiding (getFieldOffset)
import Codegen.TypeMappings (llvmType, llvmStructType)
import Data.Maybe
import LLVM.AST (Type(isPacked, StructureType), Operand, Name)
import qualified LLVM.IRBuilder.Module as L
import LLVM.AST.Name (mkName)
import Control.Monad (void)

generateStruct :: SStruct -> LLVM ()
generateStruct struct@SStruct{..} = do 
  structSignature <- llvmStructSignature struct
  registerStruct structName structSignature
  L.typedef (llvmStructName structName) (Just $ llvmStructType structSignature)
  return ()

llvmStructSignature :: MonadState (Env Operand) m => SStruct -> m StructSignature
llvmStructSignature struct@SStruct{..} = do
  fields' <- mapM (\(SVar typ name) -> do
    typ' <- llvmType typ
    let offset = fromJust $ getFieldOffset name struct
    return (FieldSignature name typ' typ offset)
    ) fields
  return (StructSignature (llvmStructName structName) fields')
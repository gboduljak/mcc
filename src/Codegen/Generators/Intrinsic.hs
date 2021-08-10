module Codegen.Generators.Intrinsic
where

import LLVM.IRBuilder
import Data.String.Conversions
import LLVM.AST.Global (Global (parameters, returnType, basicBlocks, linkage, name))
import qualified LLVM.AST.Constant as C
import Control.Monad
import LLVM.AST.Type (ptr)
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST
import LLVM.AST (
  Definition(GlobalDefinition),
  functionDefaults,
  Parameter (Parameter),
  Type (FunctionType), Operand (ConstantOperand, LocalReference))

intrinsic :: (MonadModuleBuilder m) => LLVM.AST.Name -> [(LLVM.AST.Type, ParameterName)] -> LLVM.AST.Type -> m Operand
intrinsic intrinsicName formals retTyp = do
  let formalTyps = fst <$> formals
  (formalNames, _) <-  runIRBuilderT emptyIRBuilder $ do
      forM formals $ \(_, formalName) -> case formalName of
        NoParameterName -> fresh
        ParameterName p -> fresh `named` p
  let
    intrinsicDefn = GlobalDefinition functionDefaults
      { name        = intrinsicName, 
        parameters  = (
          zipWith (\formalTyp formalName -> Parameter formalTyp formalName []) 
          formalTyps formalNames, False
        ), 
        returnType  = retTyp, 
        basicBlocks = []
      }
    intrinsicTyp = ptr $ FunctionType retTyp formalTyps False
  emitDefn intrinsicDefn
  pure $ ConstantOperand $ C.GlobalReference intrinsicTyp intrinsicName
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- For MonadState s (IRBuilderT m) instance

module Codegen.Generators.Intrinsic
where

import LLVM.IRBuilder
import Data.String.Conversions
import LLVM.AST.Global (name, Global (parameters, returnType, basicBlocks, linkage))
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
intrinsic name args retTyp = do
  let argTyps = fst <$> args
  (paramNames', _) <-  runIRBuilderT emptyIRBuilder $ do
      forM args $ \(_, paramName) -> case paramName of
        NoParameterName -> fresh
        ParameterName p -> fresh `named` p
  let
    def = GlobalDefinition functionDefaults
      { name        = name
      , parameters  = (zipWith (\ty nm -> Parameter ty nm []) argTyps paramNames', False)
      , returnType  = retTyp
      , basicBlocks = []
      }
    funcTyp = ptr $ FunctionType retTyp argTyps False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funcTyp name
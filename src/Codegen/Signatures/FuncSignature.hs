{-# LANGUAGE FlexibleContexts #-}
module Codegen.Signatures.FuncSignature where 

import qualified LLVM.AST
import Semant.Ast.SemantAst
import Control.Monad.State (MonadState)
import LLVM.AST (Type(FunctionType), Operand (ConstantOperand), Name)
import qualified Semant.Type
import Semant.Type (Type(Scalar))
import Parser.Ast (Type(StructType))
import LLVM.AST.Constant
import qualified LLVM.AST.Type
import qualified Data.Map as Map

type FuncParamSignature = (Semant.Type.Type, LLVM.AST.Type, String)

data FuncSignature = FuncSignature {
  funcName :: String,
  funcLLVMRetTyp :: LLVM.AST.Type,
  funcSemantRetTyp :: Semant.Type.Type,
  funcParams :: [FuncParamSignature],
  funcType :: LLVM.AST.Type,
  returnsStruct :: Bool
} deriving (Show, Eq)
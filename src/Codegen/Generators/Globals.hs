{-# LANGUAGE FlexibleContexts #-}
module Codegen.Generators.Globals 

where 

import Semant.Ast.SemantAst (SVarDecl (SVar))
import Codegen.Codegen
import Codegen.Generators.Statement (generateVarDecl)
import qualified LLVM.IRBuilder as L
import Codegen.Env (registerOperand, Env (Env))
import Codegen.TypeMappings (llvmType)
import Data.String.Conversions (cs)
import LLVM.AST (mkName, Operand)
import Semant.Type (Type, isInt, isChar, isDouble, isArray, isPointer)
import LLVM.AST.Constant (Constant (Int, Float, Null))
import LLVM.AST.Float (SomeFloat(Double))
import qualified LLVM.AST.Constant as LLVM.AST
import Control.Monad.State (MonadState)

generateGlobal :: SVarDecl -> LLVM ()
generateGlobal (SVar typ name) = do
  llvmTyp <- llvmType typ
  initVal <- initValue typ
  var <- L.global (mkName (cs name)) llvmTyp initVal
  registerOperand name var

initValue :: MonadState (Env Operand) m => Type -> m Constant
initValue typ 
  | isArray typ = LLVM.AST.AggregateZero <$> llvmType typ
  | isPointer typ = LLVM.AST.Constant.Null <$> llvmType typ
  | isInt typ = return (LLVM.AST.Constant.Int 32 0)
  | isChar typ = return (LLVM.AST.Constant.Int 8 0)
  | isDouble typ = return (LLVM.AST.Constant.Float (Double 0))
  | otherwise = error "semantic analysis failed on global variable"
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Codegen.Generators.Expression 

where 

import LLVM.AST (Operand)
import Codegen.Codegen
import Semant.Ast.SemantAst
import qualified LLVM.IRBuilder as L
import Data.Char
import LLVM.IRBuilder (freshName)
import Data.String.Conversions (cs)
import LLVM.AST.Constant (Constant(GlobalReference))
import LLVM.AST.Operand
import Data.Maybe (fromJust)
import SymbolTable.SymbolTable (lookupVar)

generateExpression :: SExpr -> Codegen Operand
generateExpression (_, SLitInt x) = return (L.int32 (fromIntegral x))
generateExpression (_, SLitDouble x) = return (L.double x)
generateExpression (_, SLitChar x) = return (L.int8 (fromIntegral $ ord x))
generateExpression (_, SLitString str) = do
  ptrName <- freshName (cs "str")
  strConst <- L.globalStringPtr str ptrName
  return (ConstantOperand strConst)
generateExpression (_, LVal val) = do
  ptr <- generateLVal val
  L.load ptr 0
generateExpression (_, SAssign (_, LVal val) expr) = do 
  addr <- generateLVal val 
  assignVal <- generateExpression expr 
  L.store addr 0 assignVal
  return assignVal
generateExpression (_, SCall func actualExprs) = do 
  actuals <- mapM generateExpression actualExprs
  func <- lookupFunc func
  L.call func [(act, []) | act <- actuals] -- handle pass struct byval

generateLVal :: LValue -> Codegen Operand 
generateLVal (SIdent name) = fromJust <$> lookupVar name 
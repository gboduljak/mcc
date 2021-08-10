module Codegen.Generators.Expression

where

import LLVM.AST (Operand)
import Codegen.Codegen
import Semant.Ast.SemantAst hiding (getFieldOffset)
import qualified LLVM.IRBuilder as L
import Data.Char
import LLVM.IRBuilder (freshName)
import Data.String.Conversions (cs)
import LLVM.AST.Constant (Constant(GlobalReference))
import LLVM.AST.Operand
import Data.Maybe (fromJust)
import SymbolTable.SymbolTable (lookupVar)
import qualified Semant.Type
import Codegen.Signatures.StructSignature
import Control.Monad.State
import qualified Codegen.Env
import qualified Data.Map
import qualified Data.Map as Map
import Codegen.TypeMappings (llvmType, llvmSizeOf)
import Parser.Ast (
  InfixOp (..), 
  SizeofType (SizeofType), 
  Type (StructType)
  )
import Semant.Type
    ( isPointer,
      isInt,
      isChar,
      isDouble,
      Type(Scalar, Array),
      isStruct )
import qualified LLVM.AST.Type
import qualified LLVM.AST.IntegerPredicate as L.IntegerPredicate
import qualified LLVM.AST.FloatingPointPredicate as L.FloatPredicate
import Utils.Cond ((|>), (<||>), (||>))
import Codegen.Intrinsics.Memcpy
import Codegen.Signatures.FuncSignatureLogic (llvmFuncSignature)
import Codegen.Signatures.FuncSignature (returnsStruct, FuncSignature (funcSemantRetTyp))

-- invariant #1 : every expression generator returns the operand storing the VALUE of expression result
--    Nota bene: expressions resulting in a struct or array return the operand storing 
--    the ADDRESS of expression result

generateExpression :: SExpr -> Codegen Operand
generateExpression (_, SLitInt x) = return (L.int32 (fromIntegral x))
generateExpression (_, SLitDouble x) = return (L.double x)
generateExpression (_, SLitChar x) = return (L.int8 (fromIntegral $ ord x))
generateExpression (_, SLitString str) = do
  ptrName <- freshStrLitName
  strConst <- L.globalStringPtr str ptrName
  return (ConstantOperand strConst)
generateExpression (nullTyp, SNull) = L.inttoptr (L.int64 0) =<< llvmType nullTyp
generateExpression (_, SEmptyExpr) = return (L.int32 0)
generateExpression expr@(typ, SBinop left op right) = do
  leftOp <- generateExpression left
  rightOp <- generateExpression right
  generateBinop expr leftOp rightOp
generateExpression (_, SAddressOf val) = generateLVal val
generateExpression (_, SNegate expr@(exprTyp, _))
  | isInt exprTyp = flip L.xor (L.int32 1) =<< generateExpression expr
  | isChar exprTyp = flip L.xor (L.int8 1) =<< generateExpression expr
  | isDouble exprTyp =  flip L.xor (L.double 1) =<< generateExpression expr
  | isPointer exprTyp = flip L.xor (L.int64 1) =<< generateExpression expr
  | otherwise = error ("semantic analysis failed on:" ++ show expr)
generateExpression (_, SNegative expr@(exprTyp, _))
  | isInt exprTyp = L.sub (L.int32 0) =<< generateExpression expr
  | isDouble exprTyp = L.fsub (L.double 0) =<< generateExpression expr
  | otherwise = error ("semantic analysis failed on:" ++ show expr)
generateExpression (_, SSizeof (Left (SizeofType baseTyp arraySizes))) = do
  size <- fromIntegral <$> llvmSizeOf (Array baseTyp arraySizes)
  return (L.int32 size)
generateExpression (_, SSizeof (Right (exprTyp, _))) = do
  typSize <- fromIntegral <$> llvmSizeOf exprTyp
  return (L.int32 typSize)
generateExpression expr@(targetTyp, STypecast _ srcExpr@(exprTyp, _)) = do
  expr' <- generateExpression srcExpr
  exprTyp' <- llvmType exprTyp
  targetTyp' <- llvmType targetTyp
  if targetTyp == exprTyp
    then return expr'
    else
      (|>)
      (  (targetTyp == exprTyp, return expr')
          <||> (isPointer targetTyp && isPointer exprTyp, L.bitcast expr' targetTyp')
          <||> (isPointer targetTyp && isInt exprTyp, L.inttoptr expr' targetTyp')
          <||> (isInt targetTyp && isPointer exprTyp, L.ptrtoint expr' targetTyp')
          <||> (isDouble targetTyp && isInt exprTyp, L.sitofp expr' targetTyp')
          <||> (isInt targetTyp && isChar exprTyp, L.zext expr' targetTyp')
          ||> error ("semantic analysis failed on:" ++ show expr)
      )
generateExpression (_, SCall func actualExprs) = do
  actuals <- mapM generateExpression actualExprs
  callee <- lookupFunc func
  calleeSign <- lookupFuncSignature func
  if returnsStruct calleeSign
    then do
      -- allocate space for struct on the caller stack
      structTyp <- llvmType (funcSemantRetTyp calleeSign) -- need to generate struct instead of struct*
      structSize <- fromIntegral <$> llvmSizeOf (funcSemantRetTyp calleeSign)
      structResultPtr <- L.alloca structTyp Nothing 0
      -- call
      returnedStructPtr <- generateCall callee actuals
      -- copy the returned struct to the caller stack
      performMemcpy structResultPtr returnedStructPtr (L.int64 structSize)
      return structResultPtr
    else do
      generateCall callee actuals
  
  where generateCall callee args = L.call callee [(arg, []) | arg <- args] 

generateExpression (Array _ _, LVal val) = generateLVal val
generateExpression (typ, LVal val)
 | isStruct typ = generateLVal val -- By invariant #1, if we have a struct or array, do not issue load
 | otherwise = flip L.load 0 =<< generateLVal val -- By invariant #1, for simple types, issue a load
generateExpression (typ, SAssign (_, LVal target) expr)
  | isStruct typ = generateAssignAggregate target expr -- By invariant #1, if we have a struct or array, assign is actually a copy
  | otherwise = generateAssignSimple target expr -- By invariant #1, for simple types, assign is actually a store
generateExpression (_, SAssign (_, _) expr) = error ("semantic analysis failed on:" ++ show expr)

generateBinop :: SExpr -> Operand -> Operand -> Codegen Operand
generateBinop expr@(_, SBinop left@(leftTyp, _) Add right@(rightTyp, _)) leftOp rightOp
  | isPointer leftTyp && isInt rightTyp = L.gep leftOp [rightOp]
  | isInt leftTyp && isPointer rightTyp = L.gep rightOp [leftOp]
  | isInt leftTyp = L.add leftOp rightOp
  | isChar leftTyp = L.add leftOp rightOp
  | isDouble leftTyp = L.fadd leftOp rightOp
  | otherwise = error ("semantic analysis failed on:" ++ show expr)
generateBinop expr@(_, SBinop left@(leftTyp, _) Sub right@(rightTyp, _)) leftOp rightOp
  | isPointer leftTyp && isPointer rightTyp = do
      leftOp' <- L.ptrtoint leftOp LLVM.AST.Type.i64
      rightOp' <- L.ptrtoint rightOp  LLVM.AST.Type.i64
      ptrDiff <- L.sub leftOp' rightOp'
      elemSize <- L.int64 . fromIntegral <$> llvmSizeOf leftTyp
      L.sdiv ptrDiff elemSize
  | isPointer leftTyp && isInt rightTyp = do
     leftOp' <- L.ptrtoint leftOp LLVM.AST.Type.i64
     let zero = L.int64 0
     off <- L.sub zero rightOp
     L.gep leftOp' [off]
  | isInt leftTyp && isPointer rightTyp = do
    rightOp' <- L.ptrtoint rightOp  LLVM.AST.Type.i64
    let zero = L.int64 0
    off <- L.sub zero leftOp
    L.gep rightOp' [off]
  | isInt leftTyp = L.sub leftOp rightOp
  | isChar leftTyp = L.sub leftOp rightOp
  | isDouble leftTyp = L.fsub leftOp rightOp
  | otherwise = error ("semantic analysis failed on:" ++ show expr)
generateBinop expr@(typ, SBinop _ Mul _) leftOp rightOp
  | isInt typ = L.mul leftOp rightOp
  | isChar typ = L.mul leftOp rightOp
  | isDouble typ = L.fmul leftOp rightOp
  | otherwise = error ("semantic analysis failed on:" ++ show expr)
generateBinop expr@(typ, SBinop _ Div _) leftOp rightOp
  | isInt typ = L.sdiv leftOp rightOp
  | isChar typ = L.udiv leftOp rightOp
  | isDouble typ = L.fdiv leftOp rightOp
  | otherwise = error ("semantic analysis failed on:" ++ show expr)
generateBinop expr@(typ, SBinop _ Mod _) leftOp rightOp
  | isInt typ = L.srem leftOp rightOp
  | isChar typ = L.urem leftOp rightOp
  | otherwise = error ("semantic analysis failed on:" ++ show expr)
generateBinop (_, SBinop left@(leftTyp, _) Equal _) leftOp rightOp
  | isDouble leftTyp = L.fcmp L.FloatPredicate.OEQ leftOp rightOp
  | otherwise = L.icmp L.IntegerPredicate.EQ leftOp rightOp
generateBinop (_, SBinop left@(leftTyp, _) Neq _) leftOp rightOp
  | isDouble leftTyp = L.fcmp L.FloatPredicate.ONE leftOp rightOp
  | otherwise = L.icmp L.IntegerPredicate.NE leftOp rightOp
generateBinop expr@(_, SBinop (leftTyp, _) Less _) leftOp rightOp
  | isInt leftTyp = L.icmp L.IntegerPredicate.SLT leftOp rightOp
  | isChar leftTyp = L.icmp L.IntegerPredicate.ULT leftOp rightOp
  | isDouble leftTyp = L.fcmp L.FloatPredicate.OLT leftOp rightOp
  | isPointer leftTyp = L.icmp L.IntegerPredicate.SLT leftOp rightOp
  | otherwise = error ("semantic analysis failed on:" ++ show expr)
generateBinop expr@(_, SBinop (leftTyp, _) Leq _) leftOp rightOp
  | isInt leftTyp = L.icmp L.IntegerPredicate.SLE leftOp rightOp
  | isChar leftTyp = L.icmp L.IntegerPredicate.ULE leftOp rightOp
  | isDouble leftTyp = L.fcmp L.FloatPredicate.OLE leftOp rightOp
  | isPointer leftTyp = L.icmp L.IntegerPredicate.SLE leftOp rightOp
  | otherwise = error ("semantic analysis failed on:" ++ show expr)
generateBinop expr@(_, SBinop (leftTyp, _) Greater _) leftOp rightOp
  | isInt leftTyp = L.icmp L.IntegerPredicate.SGT leftOp rightOp
  | isChar leftTyp = L.icmp L.IntegerPredicate.UGT leftOp rightOp
  | isDouble leftTyp = L.fcmp L.FloatPredicate.OGT leftOp rightOp
  | isPointer leftTyp = L.icmp L.IntegerPredicate.SGT leftOp rightOp
  | otherwise = error ("semantic analysis failed on:" ++ show expr)
generateBinop expr@(_, SBinop (leftTyp, _) Geq _) leftOp rightOp
  | isInt leftTyp = L.icmp L.IntegerPredicate.SGE leftOp rightOp
  | isChar leftTyp = L.icmp L.IntegerPredicate.UGE leftOp rightOp
  | isDouble leftTyp = L.fcmp L.FloatPredicate.OGE leftOp rightOp
  | isPointer leftTyp = L.icmp L.IntegerPredicate.SGE leftOp rightOp
  | otherwise = error ("semantic analysis failed on:" ++ show expr)
generateBinop (_, SBinop _ And _) leftOp rightOp = L.and leftOp rightOp
generateBinop (_, SBinop _ Or _) leftOp rightOp = L.or leftOp rightOp
generateBinop (_, SBinop _ BitwiseAnd _) leftOp rightOp = L.and leftOp rightOp
generateBinop (_, SBinop _ BitwiseOr _) leftOp rightOp = L.or leftOp rightOp
generateBinop (_, SBinop _ BitwiseXor _) leftOp rightOp = L.xor leftOp rightOp
generateBinop expr _ _ =  error ("binop not a binop?" ++ show expr)

-- invariant #2 : every LValue generator returns the operand storing the ADDRESS
generateLVal :: LValue -> Codegen Operand
generateLVal (SIdent name) = fromJust <$> lookupVar name
generateLVal (SDeref expr) = generateExpression expr
generateLVal (SArrayAccess expr indexExprs) = do
  accessOp <- generateExpression expr
  indexers <- mapM generateExpression indexExprs
  L.gep accessOp (L.int32 0 : indexers)
generateLVal (SFieldAccess target@(Scalar (StructType name 0), _) field) = do
  struct <- lookupStruct name
  target <- generateExpression target
  let fieldOffset = fromIntegral $ getFieldOffset field struct
  L.gep target (L.int32 0 : [L.int32 fieldOffset])
generateLVal _= error "semantic analyser failed to detect invalid lval"

generateAssignAggregate :: LValue -> SExpr -> Codegen Operand
generateAssignAggregate target expr@(typ, _) = do
  -- Nota bene: By the first invariant, expr is an operand storing 
  -- the ADDRESS of VALUE to be assigned
  -- This is a case when LVAL appears as RVAL
  destPtr <- generateLVal target
  assignPtr <- generateExpression expr
  copyBytes <- llvmSizeOf typ
  performMemcpy destPtr assignPtr (L.int64 (fromIntegral copyBytes))
  return destPtr
  
generateAssignSimple :: LValue -> SExpr -> Codegen Operand
generateAssignSimple target expr = do
  -- Nota bene: In case of assigning an expr storing the VALUE to be assigned
  -- it is correct and sufficient to simply emit a store
  addr <- generateLVal target
  assignVal <- generateExpression expr
  L.store addr 0 assignVal
  return assignVal
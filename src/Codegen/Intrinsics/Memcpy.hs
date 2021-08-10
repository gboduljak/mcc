module Codegen.Intrinsics.Memcpy 
where 

import Codegen.Codegen (LLVM, Codegen, registerFunc, lookupFunc)
import LLVM.AST (Operand (LocalReference, ConstantOperand), Type (FunctionType, argumentTypes, resultType, isVarArg), mkName, Definition (GlobalDefinition), functionDefaults, Parameter (Parameter))
import Codegen.Signatures.FuncSignature
import qualified LLVM.AST.Type
import Codegen.TypeMappings (charPtr)
import LLVM.IRBuilder ( ParameterName(NoParameterName) )
import Codegen.Generators.Intrinsic (intrinsic)
import qualified LLVM.IRBuilder as L
import LLVM.AST.Attribute (ParameterAttribute(Alignment))
import LLVM.AST.Type (i1)



memcpy :: FuncSignature
memcpy = FuncSignature {
    funcName = "llvm.memcpy.p0i8.p0i8.i64",
    funcRetTyp = memcpyRetType,
    funcParams = memcpyParams,
    funcType = FunctionType {
      resultType = memcpyRetType,
      argumentTypes = memcpyArgTypes,
      isVarArg = False
    }
  }
  where 
    memcpyRetType = LLVM.AST.Type.void
    memcpyParams = [
      (charPtr, "dest"), 
      (charPtr, "src"), 
      (LLVM.AST.Type.i64, "len"),
      (LLVM.AST.Type.i1, "isvolatile")
      ]
    memcpyArgTypes = fst <$> memcpyParams

generateMemcpy :: LLVM ()
generateMemcpy = do 
  actualFuncOperand <- intrinsic
    (mkName (funcName memcpy))
    [(paramTyp, NoParameterName) | (paramTyp, paramName) <- funcParams memcpy]
    (funcRetTyp memcpy) 
  registerFunc (funcName memcpy) actualFuncOperand
  
performMemcpy :: Operand -> Operand -> Operand -> Codegen Operand
performMemcpy destPtr sourcePtr copyBytes = do
  memcpy <- lookupFunc (funcName memcpy)
  sourcePtr' <- L.bitcast sourcePtr charPtr 
  destPtr' <- L.bitcast destPtr charPtr
  L.call memcpy [
      (destPtr', [Alignment 1]), -- structs are packed, therefore fix align
      (sourcePtr', [Alignment 1]), 
      (copyBytes, []), 
      (L.bit 0, [])
    ]
  return destPtr'
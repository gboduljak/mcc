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
import Parser.Ast (Type(PrimitiveType))
import Lexer.Lexeme (BuiltinType(Char, Int))
import Semant.Type



memcpy :: FuncSignature
memcpy = FuncSignature {
    funcName = "llvm.memcpy.p0i8.p0i8.i64",
    funcLLVMRetTyp = memcpyRetType,
    funcSemantRetTyp = Scalar (PrimitiveType Char 1),
    funcParams = memcpyParams,
    funcType = FunctionType {
      resultType = memcpyRetType,
      argumentTypes = memcpyArgTypes,
      isVarArg = False
    },
    returnsStruct = False
  }
  where 
    memcpyRetType = LLVM.AST.Type.void
    memcpyParams = [
      (Scalar (PrimitiveType Char 1), charPtr, "dest"), 
      (Scalar (PrimitiveType Char 1), charPtr, "src"), 
      (Scalar (PrimitiveType Int 0), LLVM.AST.Type.i64, "len"),
      (Any, LLVM.AST.Type.i1, "isvolatile")
      ]
    memcpyArgTypes = llvmTyp <$> memcpyParams
    llvmTyp (_, typ, _) = typ

generateMemcpy :: LLVM ()
generateMemcpy = do 
  actualFuncOperand <- intrinsic
    (mkName (funcName memcpy))
    [(paramTyp, NoParameterName) | (_, paramTyp, paramName) <- funcParams memcpy]
    (funcLLVMRetTyp memcpy) 
  registerFunc (funcName memcpy) memcpy actualFuncOperand 
  
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
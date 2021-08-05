module Codegen.Codegen 

where

import LLVM.IRBuilder (ModuleBuilderT, IRBuilderT, IRBuilderState (builderBlock), currentBlock)
import Codegen.Env
import Control.Monad.State
import LLVM.AST (Operand, Type, BasicBlock)
import qualified Semant.Type as Semant
import qualified LLVM.AST.Type as LLVM.AST
import Parser.Ast (Type(PrimitiveType))
import Lexer.Lexeme (BuiltinType(..))
import Semant.Type (Type(Scalar))

type Codegen a = IRBuilderT (ModuleBuilderT (State (Env Operand))) a


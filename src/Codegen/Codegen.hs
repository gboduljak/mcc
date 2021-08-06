{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Codegen.Codegen 

where

import LLVM.IRBuilder (ModuleBuilderT, IRBuilderT, IRBuilderState (builderBlock), currentBlock, freshName)
import Codegen.Env
import Control.Monad.State
import LLVM.AST (Operand, Type, BasicBlock, Name, Module)
import qualified Semant.Type as Semant
import qualified LLVM.AST.Type as LLVM.AST
import Parser.Ast (Type(PrimitiveType))
import Lexer.Lexeme (BuiltinType(..))
import Semant.Type (Type(Scalar))
import Data.String.Conversions
import LLVM.Prelude (ShortByteString)
import Semant.Ast.SemantAst
import Data.String (fromString)

type LLVM = ModuleBuilderT (State (Env Operand))
type Codegen = IRBuilderT LLVM

instance ConvertibleStrings String ShortByteString where
  convertString = fromString
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Codegen.Codegen 

where

import LLVM.IRBuilder (ModuleBuilderT, IRBuilderT, IRBuilderState (builderBlock), currentBlock, freshName)
import Codegen.Env
import Control.Monad.State
import qualified Data.Map as Map
import LLVM.AST (Operand, Type, BasicBlock, Name, Module)
import qualified Semant.Type as Semant
import qualified LLVM.AST.Type as LLVM.AST
import Parser.Ast (Type(PrimitiveType))
import Lexer.Lexeme (BuiltinType(..))
import Data.String.Conversions
import LLVM.Prelude (ShortByteString)
import Data.String (fromString)
import Data.Maybe (fromJust)

type LLVM = ModuleBuilderT (State (Env Operand))
type Codegen = IRBuilderT LLVM

instance ConvertibleStrings String ShortByteString where
  convertString = fromString

registerFunc :: String -> Operand -> LLVM ()
registerFunc name func = modify (\env -> env { funcs = Map.insert name func (funcs env) })

lookupFunc :: String -> Codegen Operand 
lookupFunc name = gets (fromJust . Map.lookup name . funcs) 
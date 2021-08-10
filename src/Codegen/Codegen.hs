{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Codegen.Codegen 

where

import LLVM.IRBuilder (ModuleBuilderT, IRBuilderT, IRBuilderState (builderBlock), currentBlock, freshName, ModuleBuilder)
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
import LLVM.AST.Name (mkName)
import Codegen.Signatures.StructSignature (StructSignature)
import Codegen.Signatures.FuncSignature

type LLVM = ModuleBuilderT (State (Env Operand))
type Codegen = IRBuilderT LLVM
instance ConvertibleStrings String ShortByteString where
  convertString = fromString

registerFunc ::  (MonadState (Env Operand)) m => String -> FuncSignature -> Operand -> m ()
registerFunc name signature func = do 
  modify (\env -> env { funcs = Map.insert name func (funcs env) })
  modify (\env -> env { funcSignatures = Map.insert name signature (funcSignatures env) })

lookupFuncSignature :: (MonadState (Env Operand)) m => String -> m FuncSignature 
lookupFuncSignature name = gets (fromJust . Map.lookup name . funcSignatures)

registerStruct :: String -> StructSignature  -> LLVM ()
registerStruct name struct = modify (\env -> env { structs =  Map.insert name struct (structs env) })

lookupFunc :: (MonadState (Env Operand)) m => String -> m Operand 
lookupFunc name = gets (fromJust . Map.lookup name . funcs)

lookupStruct :: (MonadState (Env Operand)) m => String -> m StructSignature
lookupStruct name = gets (fromJust . Map.lookup name . structs)

freshStrLitName :: Codegen Name
freshStrLitName = do 
  strLitId <- gets stringLitsCount
  modify (\env -> env { stringLitsCount = strLitId + 1 })
  return (mkName ("str." ++ show strLitId))
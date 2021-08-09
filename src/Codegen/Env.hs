{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Codegen.Env where

import LLVM.AST (Operand)
import Semant.Ast.SemantAst (SStruct)
import qualified Data.Map as Map (Map, fromList, empty)
import SymbolTable.ScopingEnv ( ScopingEnv(..) )
import SymbolTable.Scope (ScopeId, Scope, rootScopeId, rootScope)
import Control.Monad.RWS (MonadState, modify)
import SymbolTable.SymbolTable (defineVar)
import Control.Monad (void)
import Data.Map (Map)
import Codegen.Signatures.StructSignature (StructSignature)

data Env a = Env {
  structs :: Map String StructSignature,
  funcs :: Map String Operand,
  stringLitsCount :: Int,
  scopes :: Map ScopeId (Scope a),
  currentScopeId :: ScopeId
} deriving (Eq, Show)


emptyEnv :: Env Operand
emptyEnv = Env {
  structs = Map.empty,
  funcs = Map.empty,
  stringLitsCount = 0,
  scopes = Map.fromList [(rootScopeId, rootScope)],
  currentScopeId = rootScopeId
}

instance ScopingEnv Env where
  scopes = Codegen.Env.scopes
  currentScopeId = Codegen.Env.currentScopeId
  modifyScopes = Codegen.Env.modifyScopes
  modifyCurrentScopeId = Codegen.Env.modifyCurrentScopeId

modifyScopes :: Env a -> Map ScopeId (Scope a) -> Env a
modifyScopes Env{..} newScopes = Env {
  structs,
  funcs,
  stringLitsCount,
  scopes = newScopes,
  currentScopeId
}

modifyCurrentScopeId :: Env a -> ScopeId-> Env a
modifyCurrentScopeId Env {..} scopeId = Env {
  structs,
  funcs,
  stringLitsCount,
  scopes,
  currentScopeId = scopeId
}

registerOperand :: forall e a m .
  (
    ScopingEnv e,
    MonadState (e a) m,
    Show a,
    Eq a
  ) => String -> a -> m ()
registerOperand name op = void (defineVar (name, op))
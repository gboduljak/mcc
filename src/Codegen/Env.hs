{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Codegen.Env where

import LLVM.AST (Operand)
import Semant.Ast.SemantAst (SStruct)
import Data.Map (Map)
import SymbolTable.ScopingEnv ( ScopingEnv(..) )
import SymbolTable.Scope (ScopeId, Scope)
import Control.Monad.RWS (MonadState)
import SymbolTable.SymbolTable (defineVar)
import Control.Monad (void)

data Env a = Env {
    structs :: [SStruct],
    strings :: Map String a,
    scopes :: Map ScopeId (Scope a),
    currentScopeId :: ScopeId
} deriving (Eq, Show)

instance ScopingEnv Env where
  scopes = Codegen.Env.scopes
  currentScopeId = Codegen.Env.currentScopeId
  modifyScopes = Codegen.Env.modifyScopes
  modifyCurrentScopeId = Codegen.Env.modifyCurrentScopeId

modifyScopes :: Env a -> Map ScopeId (Scope a) -> Env a
modifyScopes Env{..} newScopes = Env {
  structs,
  strings,
  scopes = newScopes,
  currentScopeId
}

modifyCurrentScopeId :: Env a -> ScopeId-> Env a
modifyCurrentScopeId Env {..} scopeId = Env {
  structs,
  strings,
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
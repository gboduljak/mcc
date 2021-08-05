{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Semant.Env where

import Control.Monad.Identity (Identity (Identity), Monad (return))
import Control.Monad.State (MonadState (get, put), State, StateT (StateT), gets, runState)
import Data.Map (Map)
import qualified Data.Map as Map
import Parser.Ast (StructDecl (Struct), VarDecl)
import Semant.Ast.SemantAst
import Semant.Errors.SemantError
import Semant.Type (Type)
import qualified Semant.Type as St
import Prelude hiding (id, lookup)
import SymbolTable.ScopingEnv
import SymbolTable.Scope (ScopeId, Scope)

data Env a = Env
  { funcs :: Map String SFunction,
    structs :: Map String SStruct,
    scopes :: Map ScopeId (Scope a),
    currentScopeId :: ScopeId,
    bindingLoc :: BindingLoc
  }
  deriving (Show, Eq)

modifyScopes :: Env a -> Map ScopeId (Scope a) -> Env a
modifyScopes Env {..} newScopes = Env {
  funcs,
  structs,
  scopes = newScopes,
  currentScopeId,
  bindingLoc
}

modifyCurrentScopeId :: Env a -> ScopeId-> Env a
modifyCurrentScopeId Env {..} scopeId = Env {
  funcs,
  structs,
  scopes,
  currentScopeId = scopeId,
  bindingLoc
}
instance ScopingEnv Env where 
  scopes = Semant.Env.scopes
  currentScopeId = Semant.Env.currentScopeId
  modifyScopes = Semant.Env.modifyScopes
  modifyCurrentScopeId = Semant.Env.modifyCurrentScopeId
  
lookupFunc :: String -> Env St.Type -> Maybe SFunction
lookupFunc func Env {funcs} = Map.lookup func funcs

lookupStruct :: String -> Env St.Type  -> Maybe SStruct
lookupStruct struct Env {structs} = Map.lookup struct structs

defineFunc :: SFunction -> Env St.Type -> Env St.Type
defineFunc func Env {..} =
  Env
    { funcs = Map.insert (funcName func) func funcs,
      structs,
      scopes,
      currentScopeId,
      bindingLoc
    }

defineStruct :: SStruct -> Env St.Type-> Env St.Type
defineStruct struct Env {..} =
  Env
    { funcs,
      structs = Map.insert (structName struct) struct structs,
      scopes,
      currentScopeId,
      bindingLoc
    }
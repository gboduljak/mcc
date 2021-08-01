{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Semant.Env where

import Control.Monad.Identity (Identity (Identity), Monad (return))
import Control.Monad.State (MonadState (get, put), State, StateT (StateT), gets, runState)
import Data.Map (Map)
import qualified Data.Map as Map
import Parser.Ast (StructDecl (Struct), VarDecl)
import Semant.Ast.SemantAst
import Semant.Errors.SemantError
import Semant.Scope (Binding, Scope (..), ScopeId, parentId, rootScope)
import qualified Semant.Scope as Scope (extend, lookup)
import Semant.Type (Type)
import Prelude hiding (id, lookup)

data Env = Env
  { funcs :: Map String SFunction,
    structs :: Map String SStruct,
    scopes :: Map ScopeId Scope,
    currentScopeId :: ScopeId,
    bindingLoc :: BindingLoc
  }
  deriving (Show, Eq)

lookupFunc :: String -> Env -> Maybe SFunction
lookupFunc func Env {funcs} = Map.lookup func funcs

lookupStruct :: String -> Env -> Maybe SStruct
lookupStruct struct Env {structs} = Map.lookup struct structs

defineFunc :: SFunction -> Env -> Env
defineFunc func Env {..} =
  Env
    { funcs = Map.insert (funcName func) func funcs,
      structs,
      scopes,
      currentScopeId,
      bindingLoc
    }

defineStruct :: SStruct -> Env -> Env
defineStruct struct Env {..} =
  Env
    { funcs,
      structs = Map.insert (structName struct) struct structs,
      scopes,
      currentScopeId,
      bindingLoc
    }
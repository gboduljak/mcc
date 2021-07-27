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
import Semant.Scope (Binding, Scope (..), ScopeId, parentId, rootScope)
import qualified Semant.Scope as Scope (extend, lookup)
import Semant.Type (Type)
import Prelude hiding (id, lookup)

data FuncSignature = FuncSignature
  { returnType :: Type,
    funcName :: String,
    formals :: [Formal]
  }
  deriving (Show, Eq)

data Formal = Formal Type String deriving (Show, Eq)

data StructSignature = StructSignature
  { structName :: String,
    fields :: [Binding]
  }
  deriving (Show, Eq)

data Env = Env
  { funcs :: Map String FuncSignature,
    structs :: Map String StructSignature,
    scopes :: Map ScopeId Scope,
    currentScopeId :: ScopeId
  }
  deriving (Show)

lookupFunc :: String -> Env -> Maybe FuncSignature
lookupFunc func Env {funcs} = Map.lookup func funcs

lookupStruct :: String -> Env -> Maybe StructSignature
lookupStruct struct Env {structs} = Map.lookup struct structs

defineFunc :: FuncSignature -> Env -> Env
defineFunc func Env {..} =
  Env
    { funcs = Map.insert (funcName func) func funcs,
      structs,
      scopes,
      currentScopeId
    }

defineStruct :: StructSignature -> Env -> Env
defineStruct struct Env {..} =
  Env
    { funcs,
      structs = Map.insert (structName struct) struct structs,
      scopes,
      currentScopeId
    }
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Semant.Env where

import Control.Monad.Identity (Identity (Identity), Monad (return))
import Control.Monad.State (MonadState (get, put), State, StateT (StateT), gets, runState)
import Data.Map (Map)
import qualified Data.Map as Map
import Parser.Ast (Formal (Formal), Type, VarDecl)
import Semant.Scope (Binding, Scope (..), ScopeId, parentId, rootScope)
import qualified Semant.Scope as Scope (extend, lookup)
import Prelude hiding (id, lookup)

data FuncSignature = FuncSignature
  { returnType :: Type,
    funcName :: String,
    formals :: [Formal]
  }
  deriving (Show)

data StructSignature = StructSignature
  { structName :: String,
    fields :: [Binding]
  }
  deriving (Show)

data Env = Env
  { funcs :: [FuncSignature],
    structs :: [StructSignature],
    scopes :: Map ScopeId Scope,
    currentScopeId :: ScopeId
  }
  deriving (Show)

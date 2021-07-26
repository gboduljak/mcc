{-# LANGUAGE NamedFieldPuns #-}

module Semant.Scope (Scope (..), ScopeId, Binding, extend, lookup, rootScope, rootScopeId) where

import Data.Map (Map)
import qualified Data.Map as Map
import Semant.Type (Type)
import Prelude hiding (id, lookup)

type Binding = (String, Type)

type ScopeId = Int

rootScopeId :: ScopeId
rootScopeId = 0

rootScope :: Scope
rootScope =
  Scope
    { id = rootScopeId,
      parentId = Nothing,
      symbolTable = Map.empty
    }

data Scope = Scope
  { id :: ScopeId,
    parentId :: Maybe ScopeId,
    symbolTable :: Map String Binding
  }
  deriving (Show)

extend :: Scope -> Binding -> Scope
extend scope binding@(name, _) =
  Scope
    { id = id scope,
      parentId = parentId scope,
      symbolTable = Map.insert name binding (symbolTable scope)
    }

lookup :: Scope -> String -> Maybe Type
lookup Scope {symbolTable} name = snd <$> Map.lookup name symbolTable
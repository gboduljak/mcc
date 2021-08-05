{-# LANGUAGE NamedFieldPuns #-}

module SymbolTable.Scope (Scope (..), ScopeId, extend, lookup, rootScope, rootScopeId) where

import Data.Map (Map)
import qualified Data.Map as Map
import Semant.Type (Type)
import Prelude hiding (id, lookup)

type ScopeId = Int

rootScopeId :: ScopeId
rootScopeId = 0

rootScope :: Scope a
rootScope =
  Scope
    { id = rootScopeId,
      parentId = Nothing,
      symbolTable = Map.empty
    }

data Scope a = Scope
  { id :: ScopeId,
    parentId :: Maybe ScopeId,
    symbolTable :: Map String a
  }
  deriving (Show, Eq)

extend :: Scope a -> (String, a) -> Scope a
extend scope binding@(name, value) =
  Scope
    { id = id scope,
      parentId = parentId scope,
      symbolTable = Map.insert name value (symbolTable scope)
    }

lookup :: Scope a -> String -> Maybe a
lookup Scope {symbolTable} name = Map.lookup name symbolTable
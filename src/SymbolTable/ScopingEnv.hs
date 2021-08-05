{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module SymbolTable.ScopingEnv 
where 

import Data.Map as Map  (Map) 
import SymbolTable.Scope (ScopeId, Scope)

class ScopingEnv e where 
    scopes :: e a -> Map ScopeId (Scope a)
    currentScopeId :: e a -> ScopeId
    modifyScopes :: e a -> Map ScopeId (Scope a) -> e a
    modifyCurrentScopeId :: e a -> ScopeId -> e a
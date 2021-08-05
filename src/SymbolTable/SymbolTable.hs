{-# LANGUAGE RankNTypes #-}
module SymbolTable.SymbolTable

where


import SymbolTable.Scope
import Control.Monad.State (MonadState (get), gets, modify)
import qualified Data.Map as Map
import Data.Map (Map)
import Prelude hiding (id)
import qualified SymbolTable.Scope as Scope
import SymbolTable.ScopingEnv

createScope :: forall e a m .
  (
    ScopingEnv e,
    MonadState (e a) m,
    Show a,
    Eq a
  ) =>  Maybe ScopeId -> m (Scope a)
createScope parentId = do
  nextScopeId <- gets (Map.size . scopes)
  return
    ( Scope
        { id = nextScopeId,
          parentId = parentId,
          symbolTable = Map.empty
        }
    )

getScope :: forall e a m .
  (
    ScopingEnv e,
    MonadState (e a) m,
    Show a,
    Eq a
  ) => ScopeId -> m (Scope a)

getScope scopeId = gets (snd . Map.elemAt scopeId . scopes)


currentScope :: forall e a m .
  (
    ScopingEnv e,
    MonadState (e a) m,
    Show a,
    Eq a
  ) => m (Scope a)
currentScope = do
  currentScopeId <- gets currentScopeId
  getScope currentScopeId

defineVar :: forall e a m .
  (
    ScopingEnv e,
    MonadState (e a) m,
    Show a,
    Eq a
  ) => (String, a) -> m (Scope a)
defineVar binding =
  do
    scope <- currentScope
    scopes <- gets scopes
    let scope' = Scope.extend scope binding
    let newScopes = Map.insert (id scope') scope' scopes
    modify (`modifyScopes` newScopes)
    currentScope

enterScope :: forall e a m .
  (
    ScopingEnv e,
    MonadState (e a) m,
    Show a,
    Eq a
  ) =>  m (Scope a)
enterScope = do
  scopes <- gets scopes
  scopeId <- gets currentScopeId
  newScope <- createScope (Just scopeId)
  let newScopeId = id newScope
  let newScopes = Map.insert (id newScope) newScope scopes
  modify (`modifyScopes` newScopes)
  modify (`modifyCurrentScopeId` newScopeId)
  return newScope

exitScope ::  forall e a m .
  (
    ScopingEnv e,
    MonadState (e a) m,
    Show a,
    Eq a
  ) =>  m (Maybe (Scope a))
exitScope = do
  scope <- currentScope
  scopes <- gets scopes
  case parentId scope of
    (Just id) -> do
      modify (`modifyCurrentScopeId` id)
      Just <$> currentScope
    _ -> return Nothing

lookupVar :: forall e a m .
  (
    ScopingEnv e,
    MonadState (e a) m,
    Show a,
    Eq a
  ) => String -> m (Maybe a)
lookupVar name = do
  currentScopeId <- gets currentScopeId
  match <- lookupVarIn currentScopeId name
  case match of
    (Just (value, scope)) -> return (Just value)
    Nothing -> return Nothing

lookupVarIn :: forall e a m .
  (
    ScopingEnv e,
    MonadState (e a) m,
    Show a,
    Eq a
  ) => ScopeId -> String -> m (Maybe (a, ScopeId))
lookupVarIn scopeId name = do
  scope <- getScope scopeId
  case Scope.lookup scope name of
    (Just entry) -> return (Just (entry, scopeId))
    _ -> case parentId scope of
      (Just parentId) -> lookupVarIn parentId name
      _ -> return Nothing

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Semant.Semant where

import Control.Monad.State (State, evalState, gets, modify, runState)
import Control.Monad.Writer
import qualified Data.Map as Map
import Semant.Env
import Semant.Scope (Binding, Scope (Scope, id, parentId, symbolTable), ScopeId, rootScope, rootScopeId)
import qualified Semant.Scope as Scope
import Semant.SemantError
import Semant.Type (Type)
import Prelude hiding (id)

type Semant a = WriterT [SemantError] (State Env) a

runSemant :: Semant a -> Env -> a
runSemant monad env = x
  where
    (x, y) = evalState (runWriterT monad) env

getEmptyEnv :: Env
getEmptyEnv =
  Env
    { funcs = [],
      structs = [],
      scopes = Map.fromList [(rootScopeId, rootScope)],
      currentScopeId = rootScopeId
    }

createScope :: Maybe ScopeId -> Semant Scope
createScope parentId = do
  nextScopeId <- gets (Map.size . scopes)
  return
    ( Scope
        { id = nextScopeId,
          parentId = parentId,
          symbolTable = Map.empty
        }
    )

getScope :: ScopeId -> Semant Scope
getScope scopeId = do
  scopes <- gets scopes
  let currentScope = snd $ Map.elemAt scopeId scopes
   in return currentScope

current :: Semant Scope
current = do
  currentScopeId <- gets currentScopeId
  getScope currentScopeId

switchToScope :: ScopeId -> Semant Scope
switchToScope scopeId = do
  scopes <- gets scopes
  modify
    ( \Env {..} ->
        Env
          { funcs,
            structs,
            scopes,
            currentScopeId = scopeId
          }
    )
  current

extend :: Binding -> Semant Scope
extend binding =
  do
    scope <- current
    scopes <- gets scopes
    let scope' = Scope.extend scope binding
    modify
      ( \Env {..} ->
          Env
            { funcs,
              structs,
              currentScopeId = id scope,
              scopes = Map.insert (id scope) scope' scopes
            }
      )
    current

lookup :: String -> Semant (Maybe Type)
lookup name = do
  currentScopeId <- gets currentScopeId
  match <- lookupIn currentScopeId name
  case match of
    (Just (value, scope)) -> return (Just value)
    Nothing -> return Nothing

lookupIn :: ScopeId -> String -> Semant (Maybe (Type, ScopeId))
lookupIn scopeId name = do
  scope <- getScope scopeId
  case Scope.lookup scope name of
    (Just entry) -> return (Just (entry, scopeId))
    _ -> case parentId scope of
      (Just parentId) -> lookupIn parentId name
      _ -> return Nothing

enter :: Semant Scope
enter = do
  scopeId <- gets currentScopeId
  newScope <- createScope (Just scopeId)
  modify
    ( \Env {..} ->
        Env
          { funcs,
            structs,
            currentScopeId = id newScope,
            scopes = Map.insert (id newScope) newScope scopes
          }
    )
  return newScope

exit :: Semant (Maybe Scope)
exit = do
  scope <- current
  scopes <- gets scopes
  case parentId scope of
    (Just id) -> do
      modify
        ( \Env {..} ->
            Env
              { funcs,
                structs,
                currentScopeId = id,
                scopes = scopes
              }
        )
      Just <$> current
    _ -> return Nothing
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Semant.Semant where

import Control.Monad.State (MonadState (get), State, evalState, gets, modify, runState)
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Semant.Ast.SemantAst
import Semant.Env hiding (defineFunc, defineStruct, lookupFunc, lookupStruct)
import qualified Semant.Env (defineFunc, defineStruct, lookupFunc, lookupStruct)
import Semant.Errors.SemantError (BindingLoc (Toplevel), SemantError)
import Semant.Scope (Binding, Scope (Scope, id, parentId, symbolTable), ScopeId, rootScope, rootScopeId)
import qualified Semant.Scope as Scope
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
    { funcs = Map.empty,
      structs = Map.empty,
      scopes = Map.fromList [(rootScopeId, rootScope)],
      currentScopeId = rootScopeId,
      bindingLoc = Toplevel
    }

setBindingLoc :: BindingLoc -> Semant ()
setBindingLoc loc =
  modify
    ( \Env {..} ->
        Env
          { funcs,
            structs,
            scopes,
            currentScopeId,
            bindingLoc = loc
          }
    )

bindingLoc :: Semant BindingLoc
bindingLoc = gets Semant.Env.bindingLoc

registerError :: SemantError -> Semant ()
registerError error = tell [error]

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

currentScope :: Semant Scope
currentScope = do
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
            currentScopeId = scopeId,
            bindingLoc
          }
    )
  currentScope

defineVar :: Binding -> Semant Scope
defineVar binding =
  do
    scope <- currentScope
    scopes <- gets scopes
    let scope' = Scope.extend scope binding
    modify
      ( \Env {..} ->
          Env
            { funcs,
              structs,
              currentScopeId = id scope,
              scopes = Map.insert (id scope) scope' scopes,
              bindingLoc
            }
      )
    currentScope

funcs :: Semant (Map String SFunction)
funcs = gets Semant.Env.funcs

structs :: Semant (Map String SStruct)
structs = gets Semant.Env.structs

globals :: Semant (Map String SVarDecl)
globals = do
  Map.map (\(varName, varTyp) -> SVar varTyp varName)
    . symbolTable
    <$> currentScope

defineFunc :: SFunction -> Semant Env
defineFunc func = modify (Semant.Env.defineFunc func) >> get

defineStruct :: SStruct -> Semant Env
defineStruct struct = modify (Semant.Env.defineStruct struct) >> get

lookupFunc :: String -> Semant (Maybe SFunction)
lookupFunc func = gets (Semant.Env.lookupFunc func)

lookupStruct :: String -> Semant (Maybe SStruct)
lookupStruct struct = gets (Semant.Env.lookupStruct struct)

lookupVar :: String -> Semant (Maybe Type)
lookupVar name = do
  currentScopeId <- gets currentScopeId
  match <- lookupVarIn currentScopeId name
  case match of
    (Just (value, scope)) -> return (Just value)
    Nothing -> return Nothing

lookupVarIn :: ScopeId -> String -> Semant (Maybe (Type, ScopeId))
lookupVarIn scopeId name = do
  scope <- getScope scopeId
  case Scope.lookup scope name of
    (Just entry) -> return (Just (entry, scopeId))
    _ -> case parentId scope of
      (Just parentId) -> lookupVarIn parentId name
      _ -> return Nothing

enterScope :: Semant Scope
enterScope = do
  scopeId <- gets currentScopeId
  newScope <- createScope (Just scopeId)
  modify
    ( \Env {..} ->
        Env
          { funcs,
            structs,
            currentScopeId = id newScope,
            scopes = Map.insert (id newScope) newScope scopes,
            bindingLoc
          }
    )
  return newScope

exitScope :: Semant (Maybe Scope)
exitScope = do
  scope <- currentScope
  scopes <- gets scopes
  case parentId scope of
    (Just id) -> do
      modify
        ( \Env {..} ->
            Env
              { funcs,
                structs,
                currentScopeId = id,
                scopes = scopes,
                bindingLoc
              }
        )
      Just <$> currentScope
    _ -> return Nothing
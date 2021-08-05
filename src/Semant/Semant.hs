{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Semant.Semant where

import Control.Monad.State (MonadState (get), State, evalState, gets, modify, runState)
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Semant.Ast.SemantAst
import Semant.Builtins (builtins)
import Semant.Env hiding (defineFunc, defineStruct, lookupFunc, lookupStruct)
import qualified Semant.Env (defineFunc, defineStruct, lookupFunc, lookupStruct)
import qualified Semant.Env as Env
import Semant.Errors.SemantError (BindingLoc (Toplevel), SemantError)
import Semant.Type (Type)
import Prelude hiding (id)
import SymbolTable.SymbolTable (currentScope, getScope)
import SymbolTable.Scope (Scope(symbolTable), rootScopeId, rootScope)

type Semant a = WriterT [SemantError] (State (Env Type)) a

runSemant :: Semant a -> Env Type -> a
runSemant monad env = x
  where
    (x, y) = evalState (runWriterT monad) env

getEmptyEnv :: Env Type
getEmptyEnv =
  Env
    { funcs = Map.empty,
      structs = Map.empty,
      scopes = Map.fromList [(rootScopeId, rootScope)],
      currentScopeId = 0,
      bindingLoc = Toplevel
    }

getBaseEnv :: Env Type
getBaseEnv = foldr Env.defineFunc getEmptyEnv builtins

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

registerError :: SemantError -> Semant()
registerError error = tell [error]

funcs :: Semant (Map String SFunction)
funcs = gets Semant.Env.funcs

structs :: Semant (Map String SStruct)
structs = gets Semant.Env.structs

globals :: Semant (Map String SVarDecl)
globals = do
  rootScope <- getScope rootScopeId
  return $ Map.mapWithKey (flip SVar) (symbolTable rootScope)

defineFunc :: SFunction -> Semant (Env Type)
defineFunc func = modify (Semant.Env.defineFunc func) >> get

defineStruct :: SStruct -> Semant  (Env Type)
defineStruct struct = modify (Semant.Env.defineStruct struct) >> get

lookupFunc :: String -> Semant (Maybe SFunction)
lookupFunc func = gets (Semant.Env.lookupFunc func)

lookupStruct :: String -> Semant (Maybe SStruct)
lookupStruct struct = gets (Semant.Env.lookupStruct struct)
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Semant.Analysers.FuncsAnalyser where

import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import Lexer.Lexeme as L (BuiltinType (..), Lexeme (Not))
import Parser.Ast
  ( Block (..),
    Formal (Formal),
    FuncDecl (Func),
    FuncDef (FuncDef),
    Type (PrimitiveType),
    VarDecl (Var),
  )
import qualified Parser.Ast as Ast
import Semant.Analysers.ExpressionsAnalyser (analyseExpr, analyseMaybeExpr)
import Semant.Analysers.StatementsAnalyser (analyseBlock)
import Semant.Ast.SemantAst
import Semant.Errors.SemantError as E hiding (bindingLoc)
import Semant.Operators.Cond ((<||>), (|>), (||>))
import Semant.Scope (lookup)
import Semant.Semant as S
import Semant.Type (Type (Any, Array, Scalar), isChar, isCond, isDouble, isInt, isPointer)
import System.Directory.Internal.Prelude (traverse_)
import Prelude hiding (lookup)

analyseFuncDecl :: FuncDecl -> Semant SFunction
analyseFuncDecl (Func retTyp name formals) = do
  existing <- lookupFunc name
  case existing of
    (Just funcDefn) -> do
      registerError (Redeclaration name RedeclFunc)
      return funcDefn
    Nothing -> defineAndInstallFunc retTyp name formals Nothing

analyseFuncDefn :: FuncDef -> Semant SFunction
analyseFuncDefn (FuncDef retTyp name formals body) = do
  existing <- lookupFunc name
  case existing of
    (Just funcDefn@(SFunction _ _ _ (Just _))) -> do
      registerError (Redeclaration name RedeclFunc)
      return funcDefn
    (Just funcDefn@(SFunction retTyp' _ formals' Nothing)) -> do
      let defnFormals = [SFormal (Scalar formTyp) formName | (Formal formTyp formName) <- formals]
      if Scalar retTyp /= retTyp' || defnFormals /= formals'
        then do
          registerError (Redeclaration name RedeclFunc)
          return funcDefn
        else do defineAndInstallFunc retTyp name formals (Just body)
    Nothing -> defineAndInstallFunc retTyp name formals (Just body)

defineAndInstallFunc :: Ast.Type -> String -> [Formal] -> Maybe Block -> Semant SFunction
defineAndInstallFunc retTyp name formals body = do
  formals' <-
    mapM
      ( \(Formal formTyp formName) -> do
          unless (formTyp /= PrimitiveType L.Void 0) $
            registerError (VoidFormal formName name)
          return (SFormal (Scalar formTyp) formName)
      )
      formals
  setBindingLoc (FunctionBinding $ tempFuncDefn formals')
  case body of
    Nothing -> do
      let funcDefn = constructFuncDefn retTyp name formals' Nothing
      defineFunc funcDefn
      setBindingLoc Toplevel
      return funcDefn
    (Just body) -> do
      enterScope
      traverse_ (\(SFormal formTyp formName) -> defineVar (formName, formTyp)) formals'
      body' <- Just <$> analyseBlock body
      exitScope
      let funcDefn = constructFuncDefn retTyp name formals' body'
      defineFunc funcDefn
      setBindingLoc Toplevel
      return funcDefn
  where
    tempFuncDefn formals' = constructFuncDefn retTyp name formals' Nothing

constructFuncDefn :: Parser.Ast.Type -> String -> [SFormal] -> Maybe SBlock -> SFunction
constructFuncDefn retTyp name formals Nothing =
  SFunction
    { returnType = Scalar retTyp,
      funcName = name,
      formals,
      body = Nothing
    }
constructFuncDefn retTyp name formals body =
  SFunction
    { returnType = Scalar retTyp,
      funcName = name,
      formals,
      body = body
    }
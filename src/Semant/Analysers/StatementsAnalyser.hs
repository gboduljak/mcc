{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Semant.Analysers.StatementsAnalyser where

import Control.Monad (unless)
import Data.Maybe (fromJust)
import Lexer.Lexeme as L (BuiltinType (..), Lexeme (Not))
import Parser.Ast
  ( Block (..),
    Statement (BlockStatement, Expr, For, If, Return, VarDeclStatement, While),
    Type (PrimitiveType, StructType),
    VarDecl (Var),
  )
import Semant.Analysers.ExpressionsAnalyser (analyseExpr, analyseMaybeExpr)
import Semant.Ast.SemantAst
import Semant.Errors.SemantError as E hiding (bindingLoc)
import Semant.Operators.Cond ((<||>), (|>), (||>))
import Semant.Scope (lookup)
import Semant.Semant as S
import Semant.Type (Type (Any, Array, Scalar), isChar, isCond, isDouble, isInt, isPointer, voidTyp)
import Prelude hiding (lookup)

analyseBlock :: Block -> Semant SBlock
analyseBlock (Block stmts _) = do
  enterScope
  block <- SBlock <$> mapM analyseStatement stmts
  exitScope
  let flattenedStmts = flattenStmts stmts
  case followsReturn flattenedStmts of
    Just (retStmt, deadCode) -> do
      registerError (DeadCode retStmt deadCode (returnOff retStmt))
      return block
    Nothing -> return block
  where
    returnOff (Return _ off) = off

followsReturn :: [Statement] -> Maybe (Statement, [Statement])
followsReturn [] = Nothing
followsReturn [return@(Return _ _)] = Nothing
followsReturn (return@(Return _ _) : stmts) = Just (return, stmts)
followsReturn (_ : stmts) = followsReturn stmts

flattenStmts :: [Statement] -> [Statement]
flattenStmts [] = []
flattenStmts ((BlockStatement (Block stmts' _) _) : stmts) = flattenStmts stmts' ++ flattenStmts stmts
flattenStmts (stmt : stmts) = stmt : flattenStmts stmts

analyseMaybeStatement :: Maybe Statement -> Semant (Maybe SStatement)
analyseMaybeStatement Nothing = return Nothing
analyseMaybeStatement (Just stmt) = Just <$> analyseStatement stmt

analyseStatement :: Statement -> Semant SStatement
analyseStatement (Expr expr _) = SExpr <$> analyseExpr expr
analyseStatement (BlockStatement block _) = SBlockStatement <$> analyseBlock block
analyseStatement (VarDeclStatement decl _) = SVarDeclStatement <$> processVarDecl decl
analyseStatement (If cond conseq alt _) = do
  cond'@(condTyp, _) <- analyseExpr cond
  conseq' <- analyseStatement conseq
  alt' <- analyseMaybeStatement alt
  if isCond condTyp
    then do
      return (SIf cond' conseq' alt')
    else do
      registerError (TypeError ["int", "double", "char", "pointer"] condTyp cond)
      return (SIf cond' conseq' alt')
analyseStatement (Return expr off) = do
  loc <- bindingLoc
  expr'@(exprTyp, _) <- analyseMaybeExpr expr
  case loc of
    FunctionBinding {inFunc = func@(SFunction retTyp _ _ _)} ->
      if exprTyp == retTyp || exprTyp == Any
        then return (SReturn expr')
        else do
          registerError (ReturnTypeMismatchError exprTyp func expr off)
          return (SReturn expr')
    _ -> error "fatal error when processing return"
analyseStatement (While cond body _) = do
  cond'@(condTyp, _) <- analyseExpr cond
  body' <- analyseStatement body
  unless (isCond condTyp) $
    registerError (TypeError ["int", "double", "char", "pointer"] condTyp cond)
  return (rewriteAsDoWhile doNothing cond' body' doNothing)
  where
    doNothing = (Scalar (PrimitiveType L.Void 0), SEmptyExpr)
analyseStatement stmt@(For init (Just cond) incr body _) = do
  init' <- analyseMaybeExpr init
  cond'@(condTyp, _) <- analyseExpr cond
  incr' <- analyseMaybeExpr incr
  body' <- analyseStatement body
  unless (isCond condTyp) $
    registerError (TypeError ["int", "double", "char", "pointer"] condTyp cond)
  return (rewriteAsDoWhile init' cond' body' incr')
analyseStatement stmt@(For init Nothing incr body _) = do
  init' <- analyseMaybeExpr init
  incr' <- analyseMaybeExpr incr
  body' <- analyseStatement body
  return (rewriteAsDoWhile init' (voidTyp, SEmptyExpr) body' incr')
analyseStatement _ = error "fatal error when pattern matching statement"

rewriteAsDoWhile :: SExpr -> SExpr -> SStatement -> SExpr -> SStatement
rewriteAsDoWhile init' (_, SEmptyExpr) body' incr' =
  SBlockStatement
    ( blockify
        [ SExpr init',
          SDoWhile
            (Scalar (PrimitiveType Int 0), SLitInt 1)
            (SBlockStatement $ blockify [body', SExpr incr'])
        ]
    )
rewriteAsDoWhile init' cond' body' incr' =
  SBlockStatement
    ( blockify
        [ SExpr init',
          SIf
            cond'
            ( SDoWhile
                cond'
                (SBlockStatement $ blockify [body', SExpr incr'])
            )
            Nothing
        ]
    )

blockify :: [SStatement] -> SBlock
blockify stmts = SBlock [stmt | stmt <- stmts, stmt /= emptyStmt]
  where
    emptyStmt = SExpr (voidTyp, SEmptyExpr)

processVarDecl :: VarDecl -> Semant SVarDecl
processVarDecl decl@(Var typ name arraySizes off) = do
  scope <- currentScope
  loc <- bindingLoc
  case lookup scope name of
    (Just _) -> do
      registerError (IllegalBinding name Duplicate loc decl)
      return varDecl
    Nothing -> case typ of
      (PrimitiveType L.Void 0) -> do
        registerError (IllegalBinding name E.Void loc decl)
        return varDecl
      (StructType structName _) -> do
        maybeStruct <- lookupStruct structName
        case maybeStruct of
          (Just _) -> do
            defineVar (name, varDeclTyp)
            return varDecl
          _ -> do
            registerError (UndefinedSymbol structName Structure Nothing (off + 1))
            return varDecl
      _ -> do
        defineVar (name, varDeclTyp)
        return varDecl
  where
    varDeclTyp = case arraySizes of
      [] -> Scalar typ
      sizes -> Array typ sizes
    varDecl = SVar varDeclTyp name
processVarDecl _ = error "fatal error when pattern matching var decl"

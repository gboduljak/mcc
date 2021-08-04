{-# LANGUAGE LambdaCase #-}
module Semant.SemanticAnalyser where

import Parser.Ast
import Semant.Semant ( funcs, globals, structs, Semant )
import Semant.Ast.SemantAst (SProgram (SProgram))
import qualified Data.Map as Map
import Control.Monad (void)
import Semant.Analysers.StructsAnalyser (analyseStructDecl)
import Semant.Analysers.FuncsAnalyser (analyseFuncDecl, analyseFuncDefn)
import Semant.Analysers.StatementsAnalyser (processVarDecl)
import Data.Foldable (traverse_)

analyse :: Program -> Semant SProgram
analyse (Program _ constrs) = do
  traverse_
    ( \case
        (StructDecl decl) -> void (analyseStructDecl decl)
        (FuncDecl decl) -> void (analyseFuncDecl decl)
        (FuncDefn defn) -> void (analyseFuncDefn defn)
        (VarDecl decl) -> void (processVarDecl decl)
        ConstructError -> return ()
    )
    constrs
  structDefns <- Semant.Semant.structs
  funcDefns <- Semant.Semant.funcs
  globalVarDecls <- globals
  return
    ( SProgram
        (Map.elems structDefns)
        (Map.elems funcDefns)
        (Map.elems globalVarDecls)
    )
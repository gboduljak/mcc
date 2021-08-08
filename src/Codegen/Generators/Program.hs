{-# LANGUAGE BlockArguments #-}
module Codegen.Generators.Program

where

import Semant.Ast.SemantAst (SProgram (SProgram))
import qualified LLVM.AST
import Codegen.Codegen
import Codegen.Generators.Function (generateFunctionDecl, generateFunctionDefn)
import Data.Foldable (traverse_)
import Semant.Builtins (isBuiltin)

generateProgram :: SProgram -> LLVM ()
generateProgram (SProgram _ funcs _) = do 
  traverse_ generateFunctionDefn builtinFuncs
  traverse_ generateFunctionDecl customFuncs
  traverse_ generateFunctionDefn customFuncs
  where 
    builtinFuncs = [ func | func <- funcs, isBuiltin func ]
    customFuncs  = [ func | func <- funcs, (not . isBuiltin) func ]
{-# LANGUAGE BlockArguments #-}
module Codegen.Generators.Program

where

import Semant.Ast.SemantAst (SProgram (SProgram))
import qualified LLVM.AST
import Codegen.Codegen
import Codegen.Generators.Function (generateFunctionDecl, generateFunctionDefn)
import Codegen.Generators.Globals (generateGlobal)
import Data.Foldable (traverse_)
import Semant.Builtins (isBuiltin)
import Codegen.Generators.Structs (generateStruct)
import Codegen.Intrinsics.Memcpy

generateProgram :: SProgram -> LLVM ()
generateProgram (SProgram structs funcs globals) = do
  generateMemcpy
  traverse_ generateStruct structs
  traverse_ generateGlobal globals
  traverse_ generateFunctionDefn builtinFuncs
  traverse_ generateFunctionDecl customFuncs
  traverse_ generateFunctionDefn customFuncs
  where 
    builtinFuncs = [ func | func <- funcs, isBuiltin func ]
    customFuncs  = [ func | func <- funcs, (not . isBuiltin) func ]
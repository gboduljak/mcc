{-# LANGUAGE BlockArguments #-}
module Codegen.Generators.Program

where

import Semant.Ast.SemantAst (SProgram (SProgram))
import qualified LLVM.AST
import Codegen.Codegen
import Codegen.Generators.Function (generateFunction)
import Data.Foldable (traverse_)
import Debug.Trace (traceShowId)
import Semant.Builtins (isBuiltin)

generateProgram :: SProgram -> LLVM ()
generateProgram (SProgram _ funcs _) = do 
  traverse_ generateFunction builtinFuncs
  traverse_ generateFunction customFuncs
  where 
    builtinFuncs = [ func | func <- funcs, isBuiltin func]
    customFuncs  = [ func | func <- funcs, (not . isBuiltin) func]
{-# LANGUAGE BlockArguments #-}
module Codegen.Generators.Program

where

import Semant.Ast.SemantAst (SProgram (SProgram))
import qualified LLVM.AST
import Codegen.Codegen
import Codegen.Generators.Function (generateFunction)
import Data.Foldable (traverse_)
import Debug.Trace (traceShowId)

generateProgram :: SProgram -> LLVM ()
generateProgram (SProgram _ funcs _) = traverse_ generateFunction funcs
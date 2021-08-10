module Codegen.Compiler 
where 

import Codegen.Generators.Program (generateProgram)
import Semant.Ast.SemantAst (SProgram)
import qualified LLVM.AST
import LLVM.IRBuilder.Module (buildModuleT)
import Data.String.Conversions (cs)
import Control.Monad.State (evalState, evalStateT)
import Codegen.Env (emptyEnv)
import Data.String (IsString(fromString))
import Control.Monad.Identity (Identity(runIdentity))
import Data.Text.Lazy (unpack)
import LLVM.Pretty (ppllvm)
import Semant.ProgramBundle.Bundler (bundle)

compile :: String -> SProgram -> LLVM.AST.Module
compile name program = evalState (buildModuleT (cs name) (generateProgram program)) emptyEnv

compileBundle :: String -> [SProgram] -> LLVM.AST.Module 
compileBundle bundleName programs = compile bundleName (bundle programs)

emitBitcode :: String -> [SProgram] -> String
emitBitcode bundleName programs = unpack . ppllvm $ compileBundle bundleName programs
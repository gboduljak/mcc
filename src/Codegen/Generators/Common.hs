module Codegen.Generators.Common where 

import LLVM.AST.Instruction (Instruction)
import Codegen.Codegen ( Codegen )
import qualified LLVM.IRBuilder as L
import Control.Monad ( unless )

generateTerm :: Codegen () -> Codegen ()
generateTerm term = do 
  check <- L.hasTerminator
  unless check term
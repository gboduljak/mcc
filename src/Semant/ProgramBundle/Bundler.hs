module Semant.ProgramBundle.Bundler 

where 

import Semant.Ast.SemantAst (SProgram(SProgram))

bundle :: [SProgram] -> SProgram
bundle = foldr1 mergePrograms

mergePrograms :: SProgram -> SProgram -> SProgram 
mergePrograms (SProgram structs funcs decls) (SProgram structs' funcs' decls') = 
  SProgram (structs ++ structs') (funcs ++ funcs') (decls ++ decls') 
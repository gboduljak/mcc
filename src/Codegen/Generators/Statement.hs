module Codegen.Generators.Statement 

where 

import Codegen.Codegen
import Semant.Ast.SemantAst (SStatement(..), SBlock(..), SVarDecl (..), SExpr' (SEmptyExpr))
import Codegen.Generators.Expression (generateExpression)
import Control.Monad (void)
import SymbolTable.SymbolTable ( enterScope, exitScope )
import Codegen.TypeMappings (llvmType)
import LLVM.AST (mkName)
import qualified LLVM.IRBuilder as L
import Control.Monad.State
import Codegen.Env (registerOperand)

generateBlock :: SBlock  -> Codegen ()
generateBlock (SBlock stmts) = do 
  enterScope
  mapM_ generateStatement stmts
  exitScope
  return ()

generateStatement :: SStatement -> Codegen ()
generateStatement (SExpr expr) = void $ generateExpression expr
generateStatement (SBlockStatement block) = generateBlock block
generateStatement (SVarDeclStatement varDecl) = generateVarDecl varDecl
generateStatement (SDoWhile cond body) = undefined
generateStatement (SIf _ _ _) = undefined
generateStatement (SReturn (_, SEmptyExpr)) = L.retVoid 
generateStatement (SReturn expr) = do
  exp <- generateExpression expr
  L.ret exp

generateVarDecl :: SVarDecl -> Codegen ()
generateVarDecl (SVar typ name) = do
  varTyp <- lift $ llvmType typ
  varPtr <- L.alloca varTyp Nothing 0
  registerOperand name varPtr
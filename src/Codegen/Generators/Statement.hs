module Codegen.Generators.Statement 

where 

import Codegen.Codegen
import Semant.Ast.SemantAst (SStatement(..), SBlock(..), SVarDecl (..), SExpr' (SEmptyExpr))
import Codegen.Generators.Expression (generateExpression)
import Control.Monad (void)
import SymbolTable.SymbolTable ( enterScope, exitScope )
import Codegen.TypeMappings (llvmType)
import LLVM.AST (mkName, Operand (ConstantOperand))
import qualified LLVM.IRBuilder as L
import Control.Monad.State
import Codegen.Env (registerOperand)
import qualified LLVM.AST.IntegerPredicate as L.IntegerPredicate
import LLVM.IRBuilder (freshName)
import Data.String.Conversions (cs)
import Codegen.Generators.Common (generateTerm)

generateBlock :: SBlock  -> Codegen ()
generateBlock (SBlock stmts) = do 
  enterScope
  mapM_ generateStatement stmts
  exitScope
  return ()

generateMaybeStatement :: Maybe SStatement -> Codegen ()
generateMaybeStatement (Just stmt) = generateStatement stmt
generateMaybeStatement Nothing = return ()

generateStatement :: SStatement -> Codegen ()
generateStatement (SExpr expr) = void $ generateExpression expr
generateStatement (SBlockStatement block) = generateBlock block
generateStatement (SVarDeclStatement varDecl) = generateVarDecl varDecl
generateStatement (SDoWhile cond body) = undefined
generateStatement (SIf cond conseq alt) = do
  freshConseq <- freshName (cs "if.conseq")
  freshAlt <- freshName (cs "if.alt")
  freshMerge <- freshName (cs "if.merge")

  condExpr <- generateExpression cond
  thruty <- L.icmp L.IntegerPredicate.NE condExpr (L.int32 0)
  L.condBr thruty freshConseq freshAlt

  L.emitBlockStart freshConseq
  generateStatement conseq
  generateTerm (L.br freshMerge)

  L.emitBlockStart freshAlt
  generateMaybeStatement alt 
  generateTerm (L.br freshMerge)

  L.emitBlockStart freshMerge

generateStatement (SReturn (_, SEmptyExpr)) = L.retVoid 
generateStatement (SReturn expr) = do
  exp <- generateExpression expr
  L.ret exp

generateVarDecl :: SVarDecl -> Codegen ()
generateVarDecl (SVar typ name) = do
  varTyp <- lift $ llvmType typ
  varPtr <- L.alloca varTyp Nothing 0
  registerOperand name varPtr
module Codegen.Generators.Statement 

where 

import Codegen.Codegen
import Semant.Ast.SemantAst (SStatement(..), SBlock(..), SVarDecl (..), SExpr' (SEmptyExpr, SCall, SAssign))
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
generateStatement (SExpr expr@(_, SCall _ _)) = void $ generateExpression expr
generateStatement (SExpr expr@(_, SAssign _ _)) = void $ generateExpression expr
generateStatement (SExpr _) = return () -- statements of this type do not make sense
generateStatement (SBlockStatement block) = generateBlock block
generateStatement (SVarDeclStatement varDecl) = generateVarDecl varDecl
generateStatement (SDoWhile cond body) = do
  bodyName <- freshName (cs "loop.body")
  exitName <- freshName (cs "loop.exit")

  generateTerm (L.br bodyName)

  L.emitBlockStart bodyName
  generateStatement body
  condExpr <- generateExpression cond
  thruty <- L.icmp L.IntegerPredicate.NE condExpr (L.int32 0)
  generateTerm (L.condBr thruty bodyName exitName)

  L.emitBlockStart exitName

generateStatement (SIf cond conseq alt) = do
  conseqName <- freshName (cs "if.conseq")
  altName <- freshName (cs "if.alt")
  mergeName <- freshName (cs "if.merge")

  condExpr <- generateExpression cond
  thruty <- L.icmp L.IntegerPredicate.NE condExpr (L.int32 0)
  L.condBr thruty conseqName altName

  L.emitBlockStart conseqName
  generateStatement conseq
  generateTerm (L.br mergeName)

  L.emitBlockStart altName
  generateMaybeStatement alt 
  generateTerm (L.br mergeName)

  L.emitBlockStart mergeName

-- handle return struct by value, we know we are sreting so write into sret arg
generateStatement (SReturn (_, SEmptyExpr)) = L.retVoid 
generateStatement (SReturn expr) = do
  exp <- generateExpression expr
  L.ret exp

generateVarDecl :: SVarDecl -> Codegen ()
generateVarDecl (SVar typ name) = do
  varTyp <- llvmType typ
  varPtr <- L.alloca varTyp Nothing 0
  registerOperand name varPtr
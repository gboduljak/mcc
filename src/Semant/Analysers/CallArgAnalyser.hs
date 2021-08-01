module Semant.Analysers.CallArgAnalyser where

import Parser.Ast (Expr)
import Semant.Ast.SemantAst (SExpr, SFormal (SFormal))
import Semant.Errors.SemantError
import Semant.Semant
import Semant.Type

analyseArgBind :: String -> Expr -> SFormal -> SExpr -> Semant Bool
analyseArgBind callee expr _ act@(Any, _) = return False
analyseArgBind callee expr (SFormal formTyp formName) act@(actTyp, _)
  | formTyp == actTyp = return True
  | otherwise = do
    registerError (CallArgsTypeError callee formTyp formName actTyp expr)
    return False
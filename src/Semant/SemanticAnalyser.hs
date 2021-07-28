{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Semant.SemanticAnalyser where

import Lexer.Lexeme (BuiltinType (..), Lexeme)
import Parser.Ast (Expr (..), InfixOp (..), Type (PrimitiveType))
import qualified Parser.Ast as Ast
import Semant.Semant
import Semant.SemantAst
import Semant.SemantError hiding (Void)
import Semant.Type

analyseMaybeExpr :: Maybe Expr -> Semant SExpr
analyseMaybeExpr (Just expr) = analyseExpr expr
analyseMaybeExpr Nothing = return (Scalar (PrimitiveType Void 0), SNoExpr)

analyseExpr :: Expr -> Semant SExpr
analyseExpr (LitInt x) = return (Scalar (PrimitiveType Int 0), SLitInt x)
analyseExpr (LitDouble x) = return (Scalar (PrimitiveType Double 0), SLitDouble x)
analyseExpr (LitChar x) = return (Scalar (PrimitiveType Char 0), SLitChar x)
analyseExpr (LitString x) = return (Scalar (PrimitiveType Char 1), SLitString x)
analyseExpr (Sizeof (Left typ)) = return (Scalar (PrimitiveType Int 0), SSizeof (Left $ fromAstType typ))
analyseExpr (Sizeof (Right expr)) = do
  result <- analyseExpr expr
  return (Scalar (PrimitiveType Int 0), SSizeof (Right result))
analyseExpr Null = return (Scalar (PrimitiveType Void 1), SNull)
analyseExpr expr@(Ident name) = do
  result <- lookupVar name
  case result of
    (Just typ) -> return (typ, LVal (SIdent name))
    Nothing -> do
      registerError (UndefinedSymbol name Variable expr)
      return (Any, LVal (SIdent name))
analyseExpr expr@(Binop left op right) = do
  left'@(leftTyp, _) <- analyseExpr left
  right'@(rightTyp, _) <- analyseExpr right
  leftSound <- typecheckBinopArg expr left'
  rightSound <- typecheckBinopArg expr right'
  -- ensure both are sound and
  undefined

typecheckBinop :: Expr -> SExpr -> SExpr -> Semant SExpr
typecheckBinop expr@(Binop _ op _) left@(Any, _) right@(_, _) = return (Any, SBinop left op right)
typecheckBinop expr@(Binop _ op _) left@(_, _) right@(Any, _) = return (Any, SBinop left op right)
typecheckBinop expr@(Binop _ Add _) left@(leftTyp, _) right@(rightTyp, _)
  | isPointer leftTyp && isPointer rightTyp =
    registerError (BinopTypeError Add leftTyp rightTyp expr "Addition of pointers is not supported")
      >> return (Any, SBinop left Add right)
  | leftTyp == rightTyp = return (leftTyp, SBinop left Add right)
  | isPointer leftTyp && isInt rightTyp = return (leftTyp, SBinop left Add right)
  | isInt leftTyp && isPointer rightTyp = return (rightTyp, SBinop left Add right)
  | otherwise =
    registerError (BinopTypeError Add leftTyp rightTyp expr "")
      >> return (Any, SBinop left Add right)
typecheckBinop expr@(Binop _ Sub _) left@(leftTyp, _) right@(rightTyp, _)
  | isPointer leftTyp && isPointer rightTyp = return (Scalar (PrimitiveType Int 0), SBinop left Sub right)
  | leftTyp == rightTyp = return (leftTyp, SBinop left Sub right)
  | isPointer leftTyp && isInt rightTyp = return (leftTyp, SBinop left Sub right)
  | isInt leftTyp && isPointer rightTyp = return (rightTyp, SBinop left Sub right)
  | otherwise =
    registerError (BinopTypeError Sub leftTyp rightTyp expr "")
      >> return (Any, SBinop left Sub right)
typecheckBinop expr@(Binop originalLeftExp Equal originalRightExp) left@(leftTyp, leftExp) right@(rightTyp, rightExp) =
  typecheckRelational expr originalLeftExp Equal originalRightExp left right
typecheckBinop expr@(Binop originalLeftExp Neq originalRightExp) left@(leftTyp, leftExp) right@(rightTyp, rightExp) =
  typecheckRelational expr originalLeftExp Neq originalRightExp left right
typecheckBinop expr@(Binop originalLeftExp Less originalRightExp) left@(leftTyp, leftExp) right@(rightTyp, rightExp) =
  typecheckRelational expr originalLeftExp Less originalRightExp left right
typecheckBinop expr@(Binop originalLeftExp Leq originalRightExp) left@(leftTyp, leftExp) right@(rightTyp, rightExp) =
  typecheckRelational expr originalLeftExp Leq originalRightExp left right
typecheckBinop expr@(Binop originalLeftExp Greater originalRightExp) left@(leftTyp, leftExp) right@(rightTyp, rightExp) =
  typecheckRelational expr originalLeftExp Greater originalRightExp left right
typecheckBinop expr@(Binop originalLeftExp Geq originalRightExp) left@(leftTyp, leftExp) right@(rightTyp, rightExp) =
  typecheckRelational expr originalLeftExp Geq originalRightExp left right

typecheckRelational :: Expr -> Expr -> InfixOp -> Expr -> SExpr -> SExpr -> Semant SExpr
typecheckRelational originalBinop originalLeftExp relOp originalRightExp left@(leftTyp, leftExp) right@(rightTyp, rightExp) =
  case (leftExp, rightExp) of
    (SNull, _) ->
      if (not . isPointer) rightTyp
        then do
          registerError (BinopTypeError relOp leftTyp rightTyp originalBinop "Cannot compare NULL to a non-pointer.")
          return (Any, SBinop left relOp right)
        else do analyseExpr (Binop (Typecast (toAstType rightTyp) originalLeftExp) relOp originalRightExp)
    (_, SNull) ->
      if (not . isPointer) leftTyp
        then do
          registerError (BinopTypeError relOp leftTyp rightTyp originalBinop "Cannot compare NULL to a non-pointer.")
          return (Any, SBinop left relOp right)
        else do analyseExpr (Binop originalLeftExp relOp (Typecast (toAstType leftTyp) originalRightExp))
    (_, _) ->
      if leftTyp == rightTyp
        then do return (Scalar (PrimitiveType Int 0), SBinop left relOp right)
        else do
          registerError (BinopTypeError relOp leftTyp rightTyp originalBinop "Types of left and right operand need to match.")
          return (Any, SBinop left relOp right)
  where
    toAstType :: Semant.Type.Type -> Ast.Type
    toAstType (Scalar typ) = typ
    toAstType _ = error "unsupported type mapping (this should not happen)"

typecheckBinopArg :: Expr -> SExpr -> Semant Bool
typecheckBinopArg expr@(Binop _ op _) (Any, _) = return True
typecheckBinopArg expr@(Binop _ op _) (typ, _) = do
  let (rules, expected) = binopArgRules op
  if any (\rule -> rule typ) rules
    then do return True
    else do
      registerError (BinopArgTypeError op typ expr expected)
      return False

binopArgRules :: InfixOp -> ([Semant.Type.Type -> Bool], [String])
binopArgRules Add = ([isNumeric, isNonVoidPointer], ["numeric type", "non void pointer"])
binopArgRules Sub = ([isNumeric, isNonVoidPointer], ["numeric type", "non void pointer"])
binopArgRules Mul = ([isNumeric], ["numeric type"])
binopArgRules Div = ([isNumeric], ["numeric type"])
binopArgRules Mod = ([isInt, isChar], ["int", "char"])
binopArgRules Equal = ([not . isArray], ["not an array"])
binopArgRules Neq = ([not . isArray], ["not an array"])
binopArgRules Greater = ([isNumeric, isNonVoidPointer], ["numeric type", "non void pointer"])
binopArgRules Geq = ([isNumeric, isNonVoidPointer], ["numeric type", "non void pointer"])
binopArgRules Less = ([isNumeric, isNonVoidPointer], ["numeric type", "non void pointer"])
binopArgRules Leq = ([isNumeric, isNonVoidPointer], ["numeric type", "non void pointer"])
binopArgRules And = ([isNumeric, isPointer], ["numeric type", "pointer"])
binopArgRules Or = ([isNumeric, isPointer], ["numeric type", "pointer"])
binopArgRules BitwiseAnd = ([isInt, isChar, isPointer], ["int", "char", "pointer"])
binopArgRules BitwiseOr = ([isInt, isChar, isPointer], ["int", "char", "pointer"])
binopArgRules BitwiseXor = ([isInt, isChar, isPointer], ["int", "char", "pointer"])

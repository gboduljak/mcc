{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Semant.Analysers.ExpressionsAnalyser (analyseExpr, analyseMaybeExpr) where

import Control.Monad.State
import Control.Monad.Writer hiding (Any)
import Data.Foldable (traverse_)
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Lexer.Combinator.Lexer (lex')
import Lexer.Lexeme (BuiltinType (..), Lexeme (Not))
import Parser.Ast (Expr (..), InfixOp (..), Type (PrimitiveType, StructType), decreasePointerLevel, getExprOff, pointerLevel)
import qualified Parser.Ast as Ast
import Parser.AstVisualiser
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import Parser.Pratt.Parser (arraySizes, expr, parse, parseExpr, parseExprs)
import Semant.Analysers.BuiltinsAnalyser (analysePrintf, analyseScanf)
import Semant.Analysers.CallArgAnalyser (analyseArgBind)
import Semant.Ast.SemantAst
import Semant.Ast.SemantAstVisualiser (visualise, visualiseSemantAst)
import Semant.Errors.SemantError hiding (Void)
import Semant.Operators.Cond ((<||>), (|>), (||>))
import Semant.Semant
import Semant.Type
import SymbolTable.SymbolTable (lookupVar)

analyseMaybeExpr :: Maybe Expr -> Semant SExpr
analyseMaybeExpr (Just expr) = analyseExpr expr
analyseMaybeExpr Nothing = return (Scalar (PrimitiveType Void 0), SEmptyExpr)

analyseExpr :: Expr -> Semant SExpr
analyseExpr (LitInt x _) = return (Scalar (PrimitiveType Int 0), SLitInt x)
analyseExpr (LitDouble x _) = return (Scalar (PrimitiveType Double 0), SLitDouble x)
analyseExpr (LitChar x _) = return (Scalar (PrimitiveType Char 0), SLitChar x)
analyseExpr (LitString x _) = return (Scalar (PrimitiveType Char 1), SLitString x)
analyseExpr (Sizeof (Left typ) _) = return (Scalar (PrimitiveType Int 0), SSizeof (Left typ))
analyseExpr (Sizeof (Right expr) _) = do
  result <- analyseExpr expr
  return (Scalar (PrimitiveType Int 0), SSizeof (Right result))
analyseExpr (Null _) = return (Scalar (PrimitiveType Void 1), SNull)
analyseExpr (Nested expr _) = analyseExpr expr
analyseExpr expr@(Binop left op right _) = do
  (left', leftSound) <- analyseBinopArg expr left
  (right', rightSound) <- analyseBinopArg expr right
  if leftSound && rightSound
    then analyseBinop expr left' right'
    else return (Any, SBinop left' op right')
analyseExpr (Negate expr _) = do
  sexpr'@(typ, sexpr) <- analyseExpr expr
  if isNumeric typ || isPointer typ || typ == Any
    then return (typ, SNegate sexpr')
    else do
      registerError
        ( TypeError
            [ "numeric type",
              "pointer"
            ]
            typ
            expr
        )
      return (Any, SNegate sexpr')
analyseExpr (Negative expr _) = do
  sexpr'@(typ, sexpr) <- analyseExpr expr
  if isNumeric typ && (not . isChar) typ || typ == Any
    then return (typ, SNegative sexpr')
    else do
      registerError
        ( TypeError
            ["int", "double"]
            typ
            expr
        )
      return (Any, SNegative sexpr')
analyseExpr (AddressOf expr _) = do
  sexpr'@(typ, sexpr) <- analyseExpr expr
  case (typ, sexpr) of
    (Any, LVal lval) ->
      return (Any, SAddressOf sexpr')
    (Scalar (PrimitiveType typ' ptrs), LVal lval) ->
      return (Scalar (PrimitiveType typ' (ptrs + 1)), SAddressOf sexpr')
    (Scalar (StructType name ptrs), LVal lval) ->
      return (Scalar (StructType name (ptrs + 1)), SAddressOf sexpr')
    _ -> registerError (AddressError expr) >> return (Any, SAddressOf sexpr')
analyseExpr (Deref expr _) = do
  sexpr'@(typ, sexpr) <- analyseExpr expr
  if isPointer typ
    then do
      case typ of
        Any -> return (Any, LVal (SDeref sexpr'))
        (Scalar (PrimitiveType typ' ptrs)) ->
          return (Scalar (PrimitiveType typ' (ptrs - 1)), LVal (SDeref sexpr'))
        (Scalar (StructType name ptrs)) ->
          return (Scalar (StructType name (ptrs - 1)), LVal (SDeref sexpr'))
        (Array typ' _) ->
          registerError (DerefError expr) >> return (Any, LVal (SDeref sexpr'))
    else do
      registerError (DerefError expr) >> return (Any, LVal (SDeref sexpr'))
analyseExpr expr@(Ident name off) = do
  result <- lookupVar name
  case result of
    (Just typ) -> return (typ, LVal (SIdent name))
    Nothing -> do
      registerError (UndefinedSymbol name Variable (Just expr) off)
      return (Any, LVal (SIdent name))
analyseExpr expr@(FieldAccess targetExpr field off) = do
  sexpr'@(typ, sexpr) <- analyseExpr targetExpr
  case typ of
    (Scalar (StructType name 0)) -> do
      maybeStruct <- lookupStruct name
      case maybeStruct of
        (Just struct) -> case getFields field struct of
          [SVar typ _] -> return (typ, LVal (SFieldAccess sexpr' field))
          _ ->
            registerError (FieldAccessError (structName struct) expr Field)
              >> return (Any, LVal (SFieldAccess sexpr' field))
        Nothing ->
          registerError (UndefinedSymbol name Structure (Just expr) off)
            >> return (Any, LVal (SFieldAccess sexpr' field))
    _ ->
      registerError (TypeError ["struct"] typ targetExpr)
        >> return (Any, LVal (SFieldAccess sexpr' field))
analyseExpr expr@(Ast.Indirect targetExpr field off) = do
  sexpr'@(typ, sexpr) <- analyseExpr targetExpr
  case typ of
    (Scalar (StructType name 1)) -> do
      maybeStruct <- lookupStruct name
      case maybeStruct of
        (Just struct) -> case getFields field struct of
          [SVar typ' _] -> return (typ', rewriteAsDeref typ sexpr' field)
          _ ->
            registerError (FieldAccessError (structName struct) expr Field)
              >> return (Any, rewriteAsDeref typ sexpr' field)
        Nothing ->
          registerError (UndefinedSymbol name Structure (Just expr) off)
            >> return (Any, rewriteAsDeref typ sexpr' field)
    _ ->
      registerError (TypeError ["struct"] typ targetExpr)
        >> return (Any, rewriteAsDeref typ sexpr' field)
  where
    rewriteAsDeref typ accessExpr@(accessTyp, _) field =
      LVal
        ( SFieldAccess
            (Semant.Type.decreasePointerLevel accessTyp 1, LVal (SDeref accessExpr))
            field
        )
analyseExpr expr@Ast.ArrayAccess {} = do
  baseExpr'@(baseTyp, _) <- analyseExpr baseExpr
  indices' <- mapM analyseIndexExpr indexExprs
  case baseTyp of
    Any -> return (Any, LVal (SArrayAccess baseExpr' indices'))
    _ -> analyseArrayAccess expr baseExpr' indices'
  where
    (baseExpr, indexExprs) = flattenArrayAccess expr
analyseExpr expr@(Assign left right _) = do
  left'@(leftTyp, leftExpr) <- analyseExpr left
  right'@(rightTyp, _) <- analyseExpr right

  if (not . isLValue) leftExpr
    then
      registerError (AssignmentError left right)
        >> return (Any, SAssign left' right')
    else case (leftTyp, rightTyp) of
      (Any, _) -> return (Any, SAssign left' right')
      (_, Any) -> return (Any, SAssign left' right')
      (_, _) -> do
        if leftTyp == rightTyp
          then return (leftTyp, SAssign left' right')
          else
            registerError (AssignmentError left right)
              >> return (Any, SAssign left' right')
analyseExpr expr@(Typecast targetTyp right _) = do
  expr'@(exprTyp, _) <- analyseExpr right
  let targetTyp' = Scalar targetTyp
      typecast = (targetTyp', STypecast targetTyp expr')
  (|>)
    ( (exprTyp == Any, return (Any, STypecast targetTyp expr'))
        <||> (targetTyp' == exprTyp, return expr')
        <||> (isPointer targetTyp' && isPointer exprTyp, return typecast)
        <||> (isPointer targetTyp' && isInt exprTyp, return typecast)
        <||> (isInt targetTyp' && isPointer exprTyp, return typecast)
        <||> (isDouble targetTyp' && isInt exprTyp, return typecast)
        <||> (isInt targetTyp' && isChar exprTyp, return typecast)
        ||> ( registerError (CastError targetTyp' exprTyp expr)
                >> return (Any, STypecast targetTyp expr')
            )
    )
analyseExpr expr@(Call "printf" args _) = do
  args' <- mapM analyseExpr args
  case args of
    ((Ast.LitString formatString _) : formatArgs) -> analysePrintf formatString (tail args') expr
    _ -> return (Any, SCall "printf" args')
analyseExpr expr@(Call "scanf" args _) = do
  args' <- mapM analyseExpr args
  case args of
    ((Ast.LitString formatString _) : formatArgs) -> analyseScanf formatString (tail args') expr
    _ -> return (Any, SCall "scanf" args')
analyseExpr expr@(Call func args off) = do
  func' <- lookupFunc func
  args' <- mapM analyseExpr args
  case func' of
    Nothing -> do
      registerError (UndefinedSymbol func Function (Just expr) off)
      return (Any, SCall func args')
    Just SFunction {..} -> do
      if length args' == length formals
        then do
          !validArgs <- zipWithM (analyseArgBind func expr) formals args'
          if and validArgs
            then do return (returnType, SCall func args')
            else do return (Any, SCall func args')
        else do
          registerError (CallArgsNumberError func (length formals) (length args) expr)
          return (Any, SCall func args')
analyseExpr _ = error "fatal error with pattern matching expressions"

analyseArrayAccess :: Expr -> SExpr -> [SExpr] -> Semant SExpr
analyseArrayAccess astExpr baseExpr@(Any, _) indices = return (Any, LVal (SArrayAccess baseExpr indices))
analyseArrayAccess astExpr baseExpr@(Array baseTyp arraySizes, _) indices
  | length indices < length arraySizes =
    registerError (ArrayAccessError astExpr (length arraySizes) (length indices))
      >> return (Any, LVal (SArrayAccess baseExpr indices))
  | null remainingIndices =
    return (Scalar baseTyp, LVal (SArrayAccess baseExpr indices))
  | otherwise = do
    let innerAccess = (Scalar baseTyp, LVal (SArrayAccess baseExpr accessIndices))
    analyseArrayAccess astExpr innerAccess remainingIndices
  where
    (accessIndices, remainingIndices) = splitAt (length arraySizes) indices
analyseArrayAccess astExpr baseExpr@(Scalar baseTyp, _) indices
  | length indices > basePtrLevel =
    registerError (ArrayAccessError astExpr basePtrLevel (length indices))
      >> return (Any, LVal (SArrayAccess baseExpr indices))
  | otherwise = return (rewriteAsDeref (reverse indices))
  where
    rewriteAsDeref :: [SExpr] -> SExpr
    rewriteAsDeref [] = baseExpr
    rewriteAsDeref (index : indices) =
      ( Scalar (Parser.Ast.decreasePointerLevel baseTyp (length indices + 1)),
        LVal
          ( SDeref
              ( Scalar (Parser.Ast.decreasePointerLevel baseTyp (length indices)),
                SBinop (rewriteAsDeref indices) Add index
              )
          )
      )
    basePtrLevel = pointerLevel baseTyp

analyseIndexExpr :: Expr -> Semant SExpr
analyseIndexExpr expr = do
  (indexTyp, indexExpr') <- analyseExpr expr
  case indexTyp of
    Any -> return (Any, indexExpr')
    Scalar (PrimitiveType Int 0) -> return (Scalar (PrimitiveType Int 0), indexExpr')
    _ ->
      registerError (TypeError ["int"] indexTyp expr)
        >> return (Any, indexExpr')

flattenArrayAccess :: Expr -> (Expr, [Expr])
flattenArrayAccess (Ast.ArrayAccess !inner !index _) = (base, indices ++ [index])
  where
    (base, indices) = flattenArrayAccess inner
flattenArrayAccess expr = (expr, [])

analyseBinop :: Expr -> SExpr -> SExpr -> Semant SExpr
analyseBinop expr@(Binop _ op _ _) left@(Any, _) right@(_, _) = return (Any, SBinop left op right)
analyseBinop expr@(Binop _ op _ _) left@(_, _) right@(Any, _) = return (Any, SBinop left op right)
analyseBinop expr@(Binop _ Add _ _) left@(leftTyp, _) right@(rightTyp, _)
  | isPointer leftTyp && isPointer rightTyp =
    registerError (BinopTypeError Add leftTyp rightTyp expr "Addition of pointers is not supported")
      >> return (Any, SBinop left Add right)
  | leftTyp == rightTyp = return (leftTyp, SBinop left Add right)
  | isPointer leftTyp && isInt rightTyp = return (leftTyp, SBinop left Add right)
  | isInt leftTyp && isPointer rightTyp = return (rightTyp, SBinop left Add right)
  | otherwise =
    registerError (BinopTypeError Add leftTyp rightTyp expr "")
      >> return (Any, SBinop left Add right)
analyseBinop expr@(Binop _ Sub _ _) left@(leftTyp, _) right@(rightTyp, _)
  | isPointer leftTyp && isPointer rightTyp = return (Scalar (PrimitiveType Int 0), SBinop left Sub right)
  | leftTyp == rightTyp = return (leftTyp, SBinop left Sub right)
  | isPointer leftTyp && isInt rightTyp = return (leftTyp, SBinop left Sub right)
  | isInt leftTyp && isPointer rightTyp = return (rightTyp, SBinop left Sub right)
  | otherwise =
    registerError (BinopTypeError Sub leftTyp rightTyp expr "")
      >> return (Any, SBinop left Sub right)
analyseBinop expr@(Binop leftOp Equal rightOp _) left right =
  analyseRelop expr leftOp Equal rightOp left right
analyseBinop expr@(Binop leftOp Neq rightOp _) left right =
  analyseRelop expr leftOp Neq rightOp left right
analyseBinop expr@(Binop leftOp Less rightOp _) left right =
  analyseRelop expr leftOp Less rightOp left right
analyseBinop expr@(Binop leftOp Leq rightOp _) left right =
  analyseRelop expr leftOp Leq rightOp left right
analyseBinop expr@(Binop leftOp Greater rightOp _) left right =
  analyseRelop expr leftOp Greater rightOp left right
analyseBinop expr@(Binop leftOp Geq rightOp _) left right =
  analyseRelop expr leftOp Geq rightOp left right
analyseBinop expr@(Binop _ op _ _) left@(leftTyp, leftExp) right@(rightTyp, rightExp)
  | leftTyp == rightTyp = return (leftTyp, SBinop left op right)
  | otherwise =
    registerError (BinopTypeError op leftTyp rightTyp expr "Types of left and right operand need to match.")
      >> return (Any, SBinop left op right)
analyseBinop _ _ _ = error "fatal error with pattern matching binop expressions"

analyseRelop :: Expr -> Expr -> InfixOp -> Expr -> SExpr -> SExpr -> Semant SExpr
analyseRelop originalBinop leftOp relOp rightOp left@(leftTyp, leftExp) right@(rightTyp, rightExp) =
  case (leftExp, rightExp) of
    (SNull, _) ->
      if (not . isPointer) rightTyp
        then do
          registerError (BinopTypeError relOp leftTyp rightTyp originalBinop "Cannot compare NULL to a non-pointer.")
          return (Any, SBinop left relOp right)
        else analyseExpr (Binop (Typecast (toAstType rightTyp) leftOp (getExprOff leftOp)) relOp rightOp (getExprOff leftOp))
    (_, SNull) ->
      if (not . isPointer) leftTyp
        then do
          registerError (BinopTypeError relOp leftTyp rightTyp originalBinop "Cannot compare NULL to a non-pointer.")
          return (Any, SBinop left relOp right)
        else analyseExpr (Binop leftOp relOp (Typecast (toAstType leftTyp) rightOp (getExprOff leftOp)) (getExprOff leftOp))
    (_, _) ->
      if leftTyp == rightTyp
        then return (Scalar (PrimitiveType Int 0), SBinop left relOp right)
        else do
          registerError (BinopTypeError relOp leftTyp rightTyp originalBinop "Types of left and right operand need to match.")
          return (Any, SBinop left relOp right)
  where
    toAstType :: Semant.Type.Type -> Ast.Type
    toAstType (Scalar typ) = typ
    toAstType _ = error "unsupported type mapping (this should not happen)"

analyseBinopArg :: Expr -> Expr -> Semant (SExpr, Bool)
analyseBinopArg expr@(Binop _ op _ _) argExpr = do
  argExpr'@(typ, _) <- analyseExpr argExpr
  if typ == Any
    then return (argExpr', True)
    else do
      let (rules, expected) = binopArgRules op
      if any (\rule -> rule typ) rules
        then return (argExpr', True)
        else do
          registerError (BinopArgTypeError op typ expr argExpr expected)
          return (argExpr', False)
analyseBinopArg _ _ = undefined

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
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Semant.SemanticAnalyser where

import Control.Monad.State
import Control.Monad.Writer hiding (Any)
import Data.Foldable (traverse_)
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Debug.Trace
import Lexer.Combinator.Lexer (lex')
import Lexer.Lexeme (BuiltinType (..), Lexeme)
import Parser.Ast (Expr (..), InfixOp (..), Type (PrimitiveType, StructType))
import qualified Parser.Ast as Ast
import Parser.AstVisualiser
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import Parser.Pratt.Parser (expr, parse, parseExpr, parseExprs)
import Semant.Ast.SemantAst
import Semant.Ast.SemantAstVisualiser (visualise, visualiseSemantAst)
import Semant.Errors.SemantError hiding (Void)
import Semant.Semant
import Semant.Type
import System.Console.Pretty

analyseExpr' :: Ast.Expr -> Either [SemantError] SExpr
analyseExpr' expr = case result of
  (expr, []) -> Right expr
  (_, errors) -> Left errors
  where
    result = evalState (runWriterT (analyseExpr expr)) getEmptyEnv

runAnalyse' file input = do
  isPretty <- supportsPretty
  case lex' file input of
    (Right tokens) -> case parseExpr file tokens of
      (Right expr) -> case analyseExpr'' expr of
        (sast, []) -> do
          putStrLn $ visualiseAst expr
          putStrLn $ visualiseSemantAst sast
        (sast, errors) -> do
          putStrLn $ visualiseSemantAst sast
          print errors
          let displayedErrors = map prettyPrintSemantError errors

          traverse_ putStrLn displayedErrors
      (Left bundle) -> do
        print bundle
        putStrLn $ prettyPrintErrors bundle (pack input) isPretty
    (Left bundle) -> putStrLn $ prettyPrintErrors bundle (pack input) isPretty
  where
    analyseExpr'' expr = evalState (runWriterT (analyseExpr expr)) getEmptyEnv

prettyPrintSemantError :: SemantError -> String
prettyPrintSemantError = renderString . layoutSmart defaultLayoutOptions . pretty

analyseMaybeExpr :: Maybe Expr -> Semant SExpr
analyseMaybeExpr (Just expr) = analyseExpr expr
analyseMaybeExpr Nothing = return (Scalar (PrimitiveType Void 0), SEmptyExpr)

analyseExpr :: Expr -> Semant SExpr
analyseExpr (LitInt x) = return (Scalar (PrimitiveType Int 0), SLitInt x)
analyseExpr (LitDouble x) = return (Scalar (PrimitiveType Double 0), SLitDouble x)
analyseExpr (LitChar x) = return (Scalar (PrimitiveType Char 0), SLitChar x)
analyseExpr (LitString x) = return (Scalar (PrimitiveType Char 1), SLitString x)
analyseExpr (Sizeof (Left typ)) = return (Scalar (PrimitiveType Int 0), SSizeof (Left typ))
analyseExpr (Sizeof (Right expr)) = do
  result <- analyseExpr expr
  return (Scalar (PrimitiveType Int 0), SSizeof (Right result))
analyseExpr Null = return (Scalar (PrimitiveType Void 1), SNull)
analyseExpr (Nested expr) = analyseExpr expr
analyseExpr expr@(Binop left op right) = do
  (left', leftSound) <- typecheckBinopArg expr left
  (right', rightSound) <- typecheckBinopArg expr right
  if leftSound && rightSound
    then typecheckBinop expr left' right'
    else return (Any, SBinop left' op right')
analyseExpr (Negate expr) = do
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
analyseExpr (Negative expr) = do
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
analyseExpr (AddressOf expr) = do
  sexpr'@(typ, sexpr) <- analyseExpr expr
  case (typ, sexpr) of
    (Any, LVal lval) ->
      return (Any, SAddressOf sexpr')
    (Scalar (PrimitiveType typ' ptrs), LVal lval) ->
      return (Scalar (PrimitiveType typ' (ptrs + 1)), SAddressOf sexpr')
    (Scalar (StructType name ptrs), LVal lval) ->
      return (Scalar (StructType name (ptrs + 1)), SAddressOf sexpr')
    _ -> registerError (AddressError expr) >> return (Any, SAddressOf sexpr')
analyseExpr (Deref expr) = do
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
analyseExpr expr@(Ident name) = do
  result <- lookupVar (traceShowId name)
  case result of
    (Just typ) -> return (typ, LVal (SIdent name))
    Nothing -> do
      registerError (UndefinedSymbol name Variable expr)
      return (Any, LVal (SIdent name))
analyseExpr expr@(FieldAccess targetExpr field) = do
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
          registerError (UndefinedSymbol name Structure expr)
            >> return (Any, LVal (SFieldAccess sexpr' field))
    _ ->
      registerError (TypeError ["struct"] typ targetExpr)
        >> return (Any, LVal (SFieldAccess sexpr' field))
analyseExpr expr@(Ast.Indirect targetExpr field) = do
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
          registerError (UndefinedSymbol name Structure expr)
            >> return (Any, rewriteAsDeref typ sexpr' field)
    _ ->
      registerError (TypeError ["struct"] typ targetExpr)
        >> return (Any, rewriteAsDeref typ sexpr' field)
  where
    rewriteAsDeref typ accessExpr field =
      LVal
        ( SDeref
            ( typ,
              LVal (SFieldAccess accessExpr field)
            )
        )
analyseExpr expr@(Ast.ArrayAccess inner index) = do
  defineVar ("a", Scalar (PrimitiveType Int 2))
  baseExpr'@(baseTyp, _) <- analyseExpr baseExpr
  indices' <- mapM analyseIndexExpr indexExprs
  case baseTyp of
    Array scalarTyp arraySizes ->
      if length indices' < length arraySizes
        then do
          registerError (ArrayAccessError expr (length arraySizes) (length indices'))
            >> return (Any, LVal (SArrayAccess baseExpr' indices'))
        else do
          undefined
    Scalar scalarTyp -> do
      analyseArrayAccess expr baseExpr' indices'
    Any -> return (Any, LVal (SArrayAccess baseExpr' indices'))
  where
    (baseExpr, indexExprs) = flattenArrayAccess expr

analyseArrayAccess :: Expr -> SExpr -> [SExpr] -> Semant SExpr
analyseArrayAccess astExpr baseExpr@(Scalar (PrimitiveType baseTyp basePtrLevel), _) indices
  | length indices > basePtrLevel =
    registerError (ArrayAccessError astExpr basePtrLevel (length indices))
      >> return (Any, LVal (SArrayAccess baseExpr indices))
  | otherwise = return (rewriteAsDeref indices)
  where
    rewriteAsDeref :: [SExpr] -> SExpr
    rewriteAsDeref [] = baseExpr
    rewriteAsDeref (index : indices) =
      ( derefBaseTyp (length indices + 1),
        LVal
          ( SDeref
              ( derefBaseTyp (length indices),
                SBinop (rewriteAsDeref indices) Add index
              )
          )
      )
    derefBaseTyp n = Scalar (PrimitiveType baseTyp (basePtrLevel - n))
analyseArrayAccess astExpr baseExpr@(Scalar (StructType structName ptrs), _) indices = undefined
analyseArrayAccess astExpr baseExpr@(Array baseTyp arraySizes, _) indices = undefined
analyseArrayAccess astExpr baseExpr@(Any, _) indices = return (Any, LVal (SArrayAccess baseExpr indices))

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
flattenArrayAccess (Ast.ArrayAccess !inner !index) = (base, indices ++ [index])
  where
    (base, indices) = flattenArrayAccess inner
flattenArrayAccess expr = (expr, [])

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
typecheckBinop expr@(Binop leftOp Equal rightOp) left right =
  typecheckRelational expr leftOp Equal rightOp left right
typecheckBinop expr@(Binop leftOp Neq rightOp) left right =
  typecheckRelational expr leftOp Neq rightOp left right
typecheckBinop expr@(Binop leftOp Less rightOp) left right =
  typecheckRelational expr leftOp Less rightOp left right
typecheckBinop expr@(Binop leftOp Leq rightOp) left right =
  typecheckRelational expr leftOp Leq rightOp left right
typecheckBinop expr@(Binop leftOp Greater rightOp) left right =
  typecheckRelational expr leftOp Greater rightOp left right
typecheckBinop expr@(Binop leftOp Geq rightOp) left right =
  typecheckRelational expr leftOp Geq rightOp left right
typecheckBinop expr@(Binop _ op _) left@(leftTyp, leftExp) right@(rightTyp, rightExp)
  | leftTyp == rightTyp = return (leftTyp, SBinop left op right)
  | otherwise =
    registerError (BinopTypeError op leftTyp rightTyp expr "Types of left and right operand need to match.")
      >> return (Any, SBinop left op right)

typecheckRelational :: Expr -> Expr -> InfixOp -> Expr -> SExpr -> SExpr -> Semant SExpr
typecheckRelational originalBinop leftOp relOp rightOp left@(leftTyp, leftExp) right@(rightTyp, rightExp) =
  case (leftExp, rightExp) of
    (SNull, _) ->
      if (not . isPointer) rightTyp
        then do
          registerError (BinopTypeError relOp leftTyp rightTyp originalBinop "Cannot compare NULL to a non-pointer.")
          return (Any, SBinop left relOp right)
        else analyseExpr (Binop (Typecast (toAstType rightTyp) leftOp) relOp rightOp)
    (_, SNull) ->
      if (not . isPointer) leftTyp
        then do
          registerError (BinopTypeError relOp leftTyp rightTyp originalBinop "Cannot compare NULL to a non-pointer.")
          return (Any, SBinop left relOp right)
        else analyseExpr (Binop leftOp relOp (Typecast (toAstType leftTyp) rightOp))
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

typecheckBinopArg :: Expr -> Expr -> Semant (SExpr, Bool)
typecheckBinopArg expr@(Binop _ op _) argExpr = do
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
typecheckBinopArg _ _ = undefined

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

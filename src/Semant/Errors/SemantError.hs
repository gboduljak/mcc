{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Semant.Errors.SemantError where

import Data.Text.Prettyprint.Doc
import Parser.Ast
  ( Construct (StructDecl),
    Expr,
    Formal (..),
    FuncDecl (Func),
    InfixOp,
    Statement,
    StructDecl (Struct),
    VarDecl (Var),
  )
import Parser.AstPrettyPrinter
import Semant.Ast.SemantAst
import Semant.Type (Type)

data SemantError
  = IllegalBinding
      { bindingName :: String,
        bindingErrorKind :: BindingErrorKind,
        bindingLoc :: BindingLoc,
        bindingDecl :: VarDecl
      }
  | RecursiveStructDecl { 
      structDecl :: StructDecl,
      recursiveDecls :: [VarDecl],
      recursiveStructPos :: Int
    }
  | UndefinedSymbol String SymbolKind (Maybe Expr) Int
  | VoidFormal String String Int
  | BinopTypeError
      { infixOp :: InfixOp,
        leftType :: Type,
        rightType :: Type,
        binopExpr :: Expr,
        message :: String
      }
  | BinopArgTypeError
      { infixOp' :: InfixOp,
        argType :: Type,
        parentExpr :: Expr,
        argExpr :: Expr,
        expected' :: [String]
      }
  | TypeError
      { expected :: [String],
        actual :: Type,
        typeErrorExpr :: Expr
      }
  | CastError
      { to :: Type,
        from :: Type,
        castErrorExpr :: Expr
      }
  | CallArgsNumberError
      { calleeFuncName :: String,
        expectedArgsNumber :: Int,
        actualArgsNumber :: Int,
        callExpr :: Expr
      }
  | CallArgsTypeError
      { calleeFuncName' :: String,
        formalType :: Type,
        formalName :: String,
        actualType :: Type,
        callExpr' :: Expr
      }
  | Redeclaration String RedeclarationKind Int
  | EmptyProgram
  | NoMain
  | InvalidMainReturnType Type
  | AddressError Expr
  | ReturnTypeMismatchError
      { actualRetTyp :: Type,
        inFunc' :: SFunction,
        returnExpr :: Maybe Expr,
        returnOff :: Int
      }
  | DerefError Expr
  | AssignmentError
      { lhs :: Expr,
        rhs :: Expr
      }
  | FieldAccessError
      { targetStruct :: String,
        field :: Expr,
        fieldAccessErrorKind :: AccessErrorKind
      }
  | ArrayAccessError
      { targetArray :: Expr,
        expectedIndicesNumber :: Int,
        actualIndicesNumber :: Int
      }
  | DeadCode Statement [Statement] Int
  deriving (Show)

data BindingLoc
  = StructBinding {inStruct :: SStruct}
  | FunctionBinding {inFunc :: SFunction}
  | Toplevel
  deriving (Show, Eq)

data AccessErrorKind = Field | Indirect deriving (Show)

data RedeclarationKind = RedeclFunc | RedeclStruct | RedeclGlobalVar deriving (Show)

data BindingErrorKind = Duplicate | Void deriving (Show)

data SymbolKind = Variable | Function | Structure deriving (Show)

instance Pretty SemantError where
  pretty =
    \case
      EmptyProgram -> pretty "There is nothing to compile. :)"
      RecursiveStructDecl structDecl@(Struct name _ _) decls _-> 
        pretty "Struct definition error in: " <> hardline
          <+> indent indentAmount (pretty structDecl)
          <> hardline 
          <>  (
            pretty "Struct" <+> 
            pretty name <+> 
            pretty "has fields: " <+>
            hardline <>
            vcat [indent indentAmount (pretty decl) | decl <- decls] <>
            hardline <>
            pretty "of type as itself" <> 
            dot
          )
      IllegalBinding {..} ->
        pretty "Binding error: Illegal" <+> pretty bindingErrorKind
          <+> pretty "binding,"
          <+> pretty bindingName
          <+> case bindingLoc of
            FunctionBinding {inFunc = SFunction _ name _ _} -> pretty "in function" <+> pretty name
            StructBinding {inStruct = SStruct name _ _} -> pretty "in struct" <+> pretty name
            Toplevel -> pretty "at top level"
      UndefinedSymbol symbolName symbolKind referenceExpr _ ->
        pretty "Undefined" <+> pretty symbolKind <+> pretty symbolName
          <> case referenceExpr of
            (Just expr) ->
              emptyDoc
                <> comma
                <+> pretty "referenced in:"
                <+> pretty referenceExpr
                <> dot
            Nothing -> dot
      VoidFormal formName funcName _ ->
        pretty "In definition of function"
          <+> pretty funcName
          <> comma
          <+> pretty "the formal"
          <+> pretty formName
          <+> pretty "is void which is unsupported."
      BinopTypeError {..} ->
        pretty "Type error in binary expression:"
          <+> indent indentAmount (pretty binopExpr)
          <> dot
          <> hardline
          <+> pretty "Unable to apply operator "
          <+> pretty infixOp
          <+> pretty "to"
          <+> pretty "the left operand of type "
          <> pretty leftType
          <+> pretty "and the right operand of type "
          <> pretty rightType
          <> dot
          <> hardline
          <+> pretty message
      BinopArgTypeError {..} ->
        pretty "Type error in binary expression:"
          <+> ( pretty parentExpr
                  <> hardline
                  <> comma
                  <> indent indentAmount (pretty "in argument expression:" <+> pretty argExpr)
              )
          <+> hardline
          <+> pretty "Cannot apply operator"
          <+> pretty infixOp'
          <+> pretty "to"
          <+> pretty argType
          <> dot
          <> hardline
          <+> pretty "Supported types are:"
          <+> pretty expected'
          <> dot
      TypeError {..} ->
        pretty "Type error: expected one of" <+> pretty expected <+> pretty "but got"
          <+> pretty actual
          <> dot
          <+> indent
            indentAmount
            ( pretty "Error occured in expression:"
                <> hardline
                <> pretty typeErrorExpr
                <> dot
            )
      CastError {..} ->
        pretty "Cast error: can only cast between pointers, int to double, char to int, or between pointers and ints, not from"
          <+> pretty from
          <+> pretty "to"
          <+> pretty to
          <> dot
          <> hardline
          <> indent
            indentAmount
            ( pretty "Error occured in expression:" <+> pretty castErrorExpr <> dot
            )
      CallArgsNumberError {..} ->
        pretty "Argument error: function"
          <+> pretty calleeFuncName
          <+> pretty "expected"
          <+> pretty expectedArgsNumber
          <+> pretty "arguments, but was called with"
          <+> pretty actualArgsNumber
          <+> pretty "arguments"
          <> dot
          <> hardline
          <> indent
            indentAmount
            ( pretty "Error occured in call:"
                <+> pretty callExpr <> dot
            )
      CallArgsTypeError {..} ->
        pretty "Argument error: function"
          <+> pretty calleeFuncName'
          <+> pretty "expected an argument"
          <+> pretty formalName
          <+> pretty "of type"
          <+> pretty formalType
          <+> pretty "but was called with"
          <+> pretty actualType
          <> dot
          <> hardline
          <> indent
            indentAmount
            ( pretty "Error occured in call:"
                <+> pretty callExpr' <> dot
            )
      Redeclaration funcName RedeclFunc _ ->
        pretty "Error: redeclaration of function"
          <+> pretty funcName
          <> dot
      Redeclaration structName RedeclStruct _ ->
        pretty "Error: redeclaration of struct"
          <+> pretty structName
          <> dot
      Redeclaration globalVarName RedeclGlobalVar _ ->
        pretty "Error: redeclaration of global variable"
          <+> pretty globalVarName
          <> dot
      NoMain -> pretty "Error: main function not defined."
      InvalidMainReturnType typ -> 
        pretty "Error: main function has invalid return type" <> dot 
          <> hardline
          <> indent indentAmount (
            pretty "Expected main to return an int, but it returns" 
              <+> pretty typ 
              <> dot
          )
      AssignmentError lhs rhs ->
        pretty "Cannot assign" <+> pretty rhs <+> pretty "to" <+> pretty lhs <> dot
      AddressError expr ->
        pretty "Cannot take address of" <> indent indentAmount (pretty expr)
          <+> hardline
          <+> pretty "Supported addressable types are : primitive types, pointers to primitive types, struct types, pointers to struct types"
          <> dot
      DerefError expr ->
        pretty "Cannot dereference" <> indent indentAmount (pretty expr)
          <+> hardline
          <+> pretty "Supported addressable types are : primitive types, pointers to primitive types, struct types, pointers to struct types"
          <> dot
      FieldAccessError {..} ->
        pretty "Cannot access"
          <+> pretty targetStruct
          <+> pretty "with"
          <+> pretty field
          <> dot
      ReturnTypeMismatchError {..} ->
        pretty "Invalid return type"
          <+> pretty actualRetTyp
          <+> pretty "in expression:"
          <> hardline
          <+> indent indentAmount (pretty returnExpr)
          <> dot
          <+> pretty "Expected return type of function"
          <+> pretty (funcName inFunc')
          <+> pretty "is"
          <+> pretty (returnType inFunc')
          <> dot
      ArrayAccessError {..} ->
        pretty "Cannot access array"
          <+> pretty targetArray
          <+> pretty "with given indices number"
          <+> pretty actualIndicesNumber
          <> dot
          <+> pretty "Expected"
          <+> pretty expectedIndicesNumber
          <+> pretty "index expressions"
          <> dot
      DeadCode return deadStmts _ ->
        pretty "Nothing may follow a return."
          <> hardline
          <> indent indentAmount (pretty "Error occured after the statement:" <+> pretty return)
          <> hardline
          <> indent
            indentAmount
            ( pretty "Dead code is:"
                <> hardline
                <> vcat [indent indentAmount (pretty stmt) | stmt <- deadStmts]
            )

instance Pretty SymbolKind where
  pretty = \case
    Variable -> pretty "variable"
    Function -> pretty "function"
    Structure -> pretty "struct"

instance Pretty BindingErrorKind where
  pretty = \case
    Duplicate -> pretty "duplicate"
    Void -> pretty "void"
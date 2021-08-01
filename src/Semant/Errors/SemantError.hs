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
    VarDecl,
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
  | UndefinedSymbol String SymbolKind Expr
  | VoidFormal String String
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
        typeErrorExpr :: Maybe Expr
      }
  | CastError
      { to :: Type,
        from :: Type,
        castErrorExpr :: Expr
      }
  | CallArgsNumberError
      { expectedArgsNumber :: Int,
        actualArgsNumber :: Int,
        callExpr :: Expr
      }
  | CallArgsTypeError
      { formalType :: Type,
        formalName :: String,
        actualType :: Type,
        callExpr' :: Expr
      }
  | Redeclaration String RedeclarationKind
  | NoMain
  | AddressError Expr
  | ReturnTypeMismatchError
      { actualRetTyp :: Type,
        inFunc' :: SFunction,
        returnExpr :: Maybe Expr
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
  | DeadCode Statement [Statement]
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
      IllegalBinding {..} ->
        pretty "Binding Error: Illegal" <+> pretty bindingErrorKind
          <+> pretty "binding,"
          <+> pretty bindingName
          <+> case bindingLoc of
            FunctionBinding {inFunc = SFunction _ name _ _} -> pretty "in function" <+> pretty name
            StructBinding {inStruct = SStruct name _ _} -> pretty "in struct" <+> pretty name
            Toplevel -> pretty "at top level"
      UndefinedSymbol symbolName symbolKind referenceExpr ->
        pretty "Undefined" <+> pretty symbolKind <+> pretty symbolName
          <+> pretty "referenced in:" <> hardline <> pretty referenceExpr
      VoidFormal formName funcName ->
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
          <+> indent
            indentAmount
            ( pretty parentExpr
                <> hardline
                <+> pretty "in argument expression:"
                <+> indent indentAmount (pretty argExpr)
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
          <+> ( case typeErrorExpr of
                  (Just expr) ->
                    indent
                      indentAmount
                      ( pretty "Error occured in expression:"
                          <> hardline
                          <> pretty expr
                      )
                  Nothing -> emptyDoc
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
            ( pretty "Error occured in expression:" <+> pretty castErrorExpr
            )
      CallArgsNumberError {..} ->
        pretty "Argument error: function expected"
          <+> pretty expectedArgsNumber
          <+> pretty "arguments, but was called with"
          <+> pretty actualArgsNumber
          <+> pretty "arguments"
          <> dot
          <> hardline
          <> indent
            indentAmount
            ( pretty "Error occured in call:"
                <> pretty callExpr
            )
      CallArgsTypeError {..} ->
        pretty "Argument error: function expected an argument"
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
                <> pretty callExpr'
            )
      Redeclaration funcName RedeclFunc ->
        pretty "Error: redeclaration of function"
          <+> pretty funcName
          <> dot
      Redeclaration structName RedeclStruct ->
        pretty "Error: redeclaration of struct"
          <+> pretty structName
          <> dot
      Redeclaration globalVarName RedeclGlobalVar ->
        pretty "Error: redeclaration of global variable"
          <+> pretty globalVarName
          <> dot
      NoMain -> pretty "Error: main function not defined."
      AssignmentError lhs rhs ->
        pretty "Cannot assign" <+> pretty rhs <+> pretty "to" <+> pretty lhs
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
          <+> pretty "Expected "
          <+> pretty expectedIndicesNumber
          <+> pretty "index expressions."
      DeadCode return deadStmts ->
        pretty "Error: nothing may follow a return. Error occured in statement:"
          <> hardline
          <> indent indentAmount (pretty return)
          <> dot
          <> hardline
          <> pretty "Dead code is:"
          <> vcat [indent indentAmount (pretty stmt) | stmt <- deadStmts]

instance Pretty SymbolKind where
  pretty = \case
    Variable -> pretty "variable"
    Function -> pretty "function"
    Structure -> pretty "struct"

instance Pretty BindingErrorKind where
  pretty = \case
    Duplicate -> pretty "duplicate"
    Void -> pretty "void"
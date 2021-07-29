{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Semant.SemantError where

import Data.Text.Prettyprint.Doc
import Parser.Ast
  ( Construct (StructDecl),
    Expr,
    FuncDecl (Func),
    InfixOp,
    Statement,
    StructDecl (Struct),
    VarDecl,
  )
import Parser.AstPrettyPrinter
import Semant.Type (Type)

data SemantError
  = IllegalBinding
      { bindingName :: String,
        bindingErrorKind :: BindingErrorKind,
        bindingLoc :: BindingLoc
      }
  | UndefinedSymbol String SymbolKind Expr
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
  | CallArgsError
      { expectedArgsNumber :: Int,
        actualArgsNumber :: Int,
        callExpr :: Expr
      }
  | Redeclaration String RedeclarationKind
  | NoMain
  | AddressError Expr
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
  | DeadCode Statement
  deriving (Show)

data BindingLoc
  = StructBinding {inStruct :: StructDecl, bindingDecl :: VarDecl}
  | FunctionBinding {inFunc :: FuncDecl, bindingDecl :: VarDecl}
  | Toplevel
  deriving (Show)

data AccessErrorKind = Field | Indirect deriving (Show)

data RedeclarationKind = RedeclFunc | RedeclStruct | RedeclGlobalVar deriving (Show)

data BindingErrorKind = Duplicate | Void deriving (Show)

data SymbolKind = Variable | Function deriving (Show)

instance Pretty SemantError where
  pretty =
    \case
      IllegalBinding {..} ->
        pretty "Binding Error: Illegal" <+> pretty bindingErrorKind
          <+> pretty "binding,"
          <+> pretty bindingName
          <+> case bindingLoc of
            FunctionBinding {inFunc = Func _ name _} -> pretty "in function" <+> pretty name
            StructBinding {inStruct = Struct name _} -> pretty "in struct" <+> pretty name
            Toplevel -> pretty "at top level"
      UndefinedSymbol symbolName symbolKind referenceExpr ->
        pretty "Undefined" <+> pretty symbolKind <+> pretty symbolName
          <+> pretty "referenced in:" <> hardline <> pretty referenceExpr
      BinopTypeError {..} ->
        pretty "Type error in binary expression:"
          <+> indent indentAmount (pretty binopExpr)
          <+> dot
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
          <+> dot
      TypeError {..} ->
        pretty "Type error: expected one of" <+> pretty expected <+> pretty "but got"
          <+> pretty actual
          <+> dot
          <> indent
            indentAmount
            ( pretty " Error occured in expression:"
                <> hardline
                <> pretty typeErrorExpr
                <> hardline
            )
      CastError {..} ->
        pretty "Cast error: can only cast between pointers, between ints and doubles, or between pointers and ints, not from"
          <+> pretty from
          <+> pretty "to"
          <+> pretty to
          <+> dot
          <> indent
            indentAmount
            ( pretty "Error occured in statement:"
                <> hardline
                <> indent
                  indentAmount
                  ( pretty ", in expression: "
                      <> pretty castErrorExpr
                  )
            )
      CallArgsError {..} ->
        pretty "Argument error: function expected"
          <+> pretty expectedArgsNumber
          <+> pretty "arguments, but was called with"
          <+> pretty actualArgsNumber
          <+> pretty "arguments"
          <+> dot
          <> indent
            indentAmount
            ( pretty "Error occured in call:"
                <> hardline
                <> pretty callExpr
            )
      Redeclaration funcName RedeclFunc ->
        pretty "Error: redeclaration of function"
          <+> pretty funcName
          <+> dot
      Redeclaration structName RedeclStruct ->
        pretty "Error: redeclaration of struct"
          <+> pretty structName
          <+> dot
      Redeclaration globalVarName RedeclGlobalVar ->
        pretty "Error: redeclaration of global variable"
          <+> pretty globalVarName
          <+> dot
      NoMain -> pretty "Error: main function not defined."
      AssignmentError lhs rhs ->
        pretty "Cannot assign" <+> pretty rhs <+> pretty "to" <+> pretty lhs
      AddressError expr -> pretty "Cannot take address of" <> indent indentAmount (pretty expr)
      FieldAccessError {..} ->
        pretty "Cannot access"
          <+> pretty targetStruct
          <+> pretty "with"
          <+> pretty field
      ArrayAccessError {..} ->
        pretty "Cannot access array"
          <+> pretty targetArray
          <+> pretty "with given indices number"
          <+> pretty actualIndicesNumber
          <> dot
          <+> pretty "Expected "
          <+> pretty expectedIndicesNumber
          <+> pretty "index expressions."
      DeadCode stmt ->
        pretty "Error: nothing may follow a return. Error occured in statement:"
          <> hardline
          <> pretty stmt

instance Pretty SymbolKind where
  pretty = \case
    Variable -> pretty "variable"
    Function -> pretty "function"

instance Pretty BindingErrorKind where
  pretty = \case
    Duplicate -> pretty "duplicate"
    Void -> pretty "void"
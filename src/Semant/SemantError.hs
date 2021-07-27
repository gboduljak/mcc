{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Semant.SemantError where

import Data.Text.Prettyprint.Doc
import Lexer.Lexeme (Lexeme (Type))
import Parser.Ast
  ( Construct (StructDecl),
    Expr,
    FuncDecl (Func),
    Statement,
    StructDecl (Struct),
    Type,
    VarDecl,
  )
import Parser.AstPrettyPrinter

data SemantError
  = IllegalBinding
      { bindingName :: String,
        bindingErrorKind :: BindingErrorKind,
        bindingLoc :: BindingLoc
      }
  | UndefinedSymbol String SymbolKind Expr
  | TypeError
      { expected :: [Type],
        actual :: Type,
        typeErrorStmt :: Statement,
        typeErrorExpr :: Expr
      }
  | CastError
      { to :: Type,
        from :: Type,
        castErrorStmt :: Statement,
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
      TypeError {..} ->
        pretty "Type error: expected one of" <+> pretty expected <+> pretty "but got"
          <+> pretty actual
          <+> dot
          <> indent
            indentAmount
            ( pretty " Error occured in statement:"
                <> hardline
                <> pretty typeErrorStmt
                <> hardline
                <> indent
                  indentAmount
                  ( pretty ", in expression: "
                      <> pretty typeErrorExpr
                  )
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
                <> pretty castErrorStmt
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
{-# LANGUAGE NamedFieldPuns #-}

module Semant.Analysers.StructsAnalyser where

import Control.Monad.State
import Control.Monad.Writer hiding (Any)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Debug.Trace
import Lexer.Combinator.Lexer (lex')
import Lexer.Lexeme (BuiltinType (..), Lexeme (Not))
import Parser.Ast (Expr (..), InfixOp (..), StructDecl (Struct), Type (PrimitiveType, StructType), VarDecl (Var), decreasePointerLevel, pointerLevel)
import qualified Parser.Ast as Ast
import Parser.AstVisualiser
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import Parser.Pratt.Parser (arraySizes, expr, parse, parseExpr, parseExprs)
import Semant.Analysers.StatementsAnalyser
import Semant.Ast.SemantAst
import Semant.Ast.SemantAstVisualiser (visualise, visualiseSemantAst)
import Semant.Errors.SemantError hiding (Void)
import Semant.Semant
import Semant.Type
import SymbolTable.SymbolTable (enterScope, exitScope)
import Data.List (elem)

analyseStructDecl :: StructDecl -> Semant (Maybe SStruct)
analyseStructDecl structDecl@(Struct structName fieldDecls pos) = do
  existing <- lookupStruct structName
  case existing of
    (Just _) -> do
      registerError (Redeclaration structName RedeclStruct pos)
      return (
        Just
          ( SStruct
              { structName,
                fields = [],
                fieldOffsets = Map.empty
              }
          )
        )
    Nothing -> do
      defineStruct initialStructDefn
      setBindingLoc initialStructBinding
      enterScope
      fields <- mapM processVarDecl fieldDecls
      exitScope
      setBindingLoc Toplevel

      if (not . null) invalidDecls 
        then do 
          registerError (RecursiveStructDecl structDecl invalidDecls (pos - 1))
          return Nothing
        else do 
          let struct =
                SStruct
                  { structName,
                    fields,
                    fieldOffsets = Map.fromList . zip (map varName fields) $ [0 ..]
                  }
          defineStruct struct
          return (Just struct)
  where
    initialStructDefn =
      ( SStruct
          { structName,
            fields = [],
            fieldOffsets = Map.empty
          }
      )
    initialStructBinding = StructBinding initialStructDefn
    invalidDecls = [ 
      decl | decl@(Var fieldType _ _ _) <- fieldDecls, 
      fieldType == StructType structName 0 
      ]
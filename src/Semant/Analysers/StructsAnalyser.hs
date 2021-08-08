{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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
import Parser.Ast (Expr (..), InfixOp (..), StructDecl (Struct), Type (PrimitiveType, StructType), VarDecl, decreasePointerLevel, pointerLevel)
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

analyseStructDecl :: StructDecl -> Semant SStruct
analyseStructDecl (Struct structName fieldDecls pos) = do
  existing <- lookupStruct structName
  case existing of
    (Just _) -> do
      registerError (Redeclaration structName RedeclStruct pos)
      return
        ( SStruct
            { structName,
              fields = [],
              fieldOffsets = Map.empty
            }
        )
    Nothing -> do
      defineStruct initialStructDefn
      setBindingLoc initialStructBinding
      enterScope
      fields <- mapM processVarDecl fieldDecls
      exitScope
      setBindingLoc Toplevel
      let struct =
            SStruct
              { structName,
                fields,
                fieldOffsets = Map.fromList . zip (map varName fields) $ [0 ..]
              }
      defineStruct struct
      return struct
  where
    initialStructDefn =
      ( SStruct
          { structName,
            fields = [],
            fieldOffsets = Map.empty
          }
      )
    initialStructBinding = StructBinding initialStructDefn
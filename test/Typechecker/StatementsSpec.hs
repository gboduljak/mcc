module Typechecker.StatementsSpec where

import Data.Either
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import GHC.IO.Buffer (checkBuffer)
import qualified Lexer.Combinator.Lexer as CombinatorLexer (lex')
import Lexer.Lexeme (BuiltinType (..))
import Lexer.Token (Token (lexeme))
import Parser.Ast as Ast
import Parser.AstPrettyPrinter
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import qualified Parser.Pratt.Parser as PrattParser (parseStatements)
import Semant.Ast.SemantAst as SAst hiding (funcs, structs)
import Semant.Env (Env (..))
import Semant.Errors.SemantError (BindingLoc (..))
import SymbolTable.Scope (Scope (..), rootScopeId, symbolTable)
import Semant.Exports (analyseStmtStateful)
import Semant.Type
import System.Console.Pretty (supportsPretty)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension)
import Test.Hspec (Spec, SpecWith, describe, expectationFailure, it, shouldBe)
import Prelude hiding (id, lex)
import Typechecker.Utils.SpecUtils (getStmtSpec, lex, prettyPrintStatement)

statementsPassingSpec :: Spec
statementsPassingSpec =
   describe specDesc $ do
    it testDesc $ do
      stmtsInput <- readFile path
      let tokens = lex "" stmtsInput
      case PrattParser.parseStatements "" tokens of
        (Left errorBundle) -> do
          expectationFailure "with pratt, expected successfull parse of exprs"
        (Right stmts) -> do
          traverse_  (putStrLn . prettyPrintStatement) stmts
          let rootStmt = BlockStatement (Block stmts 0) 0
          isRight (analyseStmtStateful rootStmt testEnv) `shouldBe` True
  where
    specDesc = "typechecks statements..."
    testDesc = "correctly typechecks statements"
    path = "./test/tests-cases/typechecking/statements-passes.txt"


testEnv :: Env Semant.Type.Type
testEnv =
  Env
    { funcs = Map.empty,
      structs = Map.empty,
      scopes = Map.fromList [(rootScopeId, rootScope)],
      currentScopeId = rootScopeId,
      bindingLoc = Toplevel
    }

rootScope :: Scope Semant.Type.Type
rootScope =
  Scope
    { id = rootScopeId,
      parentId = Nothing,
      symbolTable = Map.fromList [
        ("a", Scalar (PrimitiveType Int 0))
      ]
    }
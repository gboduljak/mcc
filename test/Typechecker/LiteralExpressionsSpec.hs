module Typechecker.LiteralExpressionsSpec where

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
import qualified Parser.Pratt.Parser as PrattParser (parseExprs)
import Semant.Ast.SemantAst as SAst hiding (funcs, structs)
import Semant.Env (Env (..))
import Semant.Errors.SemantError (BindingLoc (..))
import Semant.Scope (Scope (..), rootScopeId, symbolTable)
import Semant.Exports (analyseExpr, analyseExprStateful)
import Semant.Type
import System.Console.Pretty (supportsPretty)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension)
import Test.Hspec (Spec, SpecWith, describe, expectationFailure, it, shouldBe)
import Prelude hiding (id, lex)
import Typechecker.Utils.SpecUtils (getExprSpec)

typechecksLiteralExpressionsSpec :: Spec
typechecksLiteralExpressionsSpec = getExprSpec specDesc testDesc path test
  where
    specDesc = "typechecks literal expressions..."
    testDesc = "correctly typechecks literal expressions"
    path = "./test/tests-cases/typechecking/literal-expressions-passes.txt"
    test = \expr -> (isRight . analyseExpr) expr `shouldBe` True

typechecksFailingLiteralExpressionsSpec :: Spec
typechecksFailingLiteralExpressionsSpec = getExprSpec specDesc testDesc path test
  where
    specDesc = "typechecks literal expressions..."
    testDesc = "correctly typechecks failing literal expressions"
    path = "./test/tests-cases/typechecking/literal-expressions-failures.txt"
    test = \expr -> (isRight . analyseExpr) expr `shouldBe` False

typechecksStatefulExpressionsSpec :: Spec
typechecksStatefulExpressionsSpec = getExprSpec specDesc testDesc path test
  where
    specDesc = "typechecks literal expressions..."
    testDesc = "correctly typechecks failing literal expressions"
    path = "./test/tests-cases/typechecking/stateful-expressions-passes.txt"
    test = \expr -> isRight (analyseExprStateful expr testEnv) `shouldBe` True

    testEnv :: Env
    testEnv =
      Env
        { funcs = Map.empty,
          structs = Map.empty,
          scopes = Map.fromList [(rootScopeId, rootScope)],
          currentScopeId = rootScopeId,
          bindingLoc = Toplevel
        }
    rootScope :: Scope
    rootScope =
      Scope
        { id = rootScopeId,
          parentId = Nothing,
          symbolTable = Map.fromList [("a", ("a", Scalar (PrimitiveType Int 0)))]
        }
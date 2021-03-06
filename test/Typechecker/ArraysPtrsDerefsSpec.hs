module Typechecker.ArraysPtrsDerefsSpec where
import Typechecker.Utils.SpecUtils
import Semant.Ast.SemantAst as SAst hiding (funcs, structs)
import Semant.Env (Env (..))
import Semant.Errors.SemantError (BindingLoc (..))
import SymbolTable.Scope (Scope (..), rootScopeId, symbolTable)
import Semant.Exports (analyseStmtStateful)
import qualified Data.Map as Map
import Semant.Type
import Lexer.Lexeme (BuiltinType (..))
import Lexer.Lexeme
import Parser.Ast as Ast
import Prelude hiding (id)
import Data.Maybe (isJust)
import Data.Either (isRight, isLeft)
import Test.Hspec

typechecksArraysPtrsDerefsPassingSpec :: Spec
typechecksArraysPtrsDerefsPassingSpec = getStmtSpec specDesc testDesc path test
  where
    specDesc = "typechecks arrays, ptrs and derefs ..."
    testDesc = "correctly typechecks passing arrays, ptrs and derefs"
    path = "./test/tests-cases/typechecking/arrays-ptrs-derefs-passing.txt"
    test = \expr -> isRight (analyseStmtStateful expr testEnv) `shouldBe` True

typechecksArraysPtrsDerefsFailingSpec :: Spec
typechecksArraysPtrsDerefsFailingSpec = getStmtSpec specDesc testDesc path test
  where
    specDesc = "typechecks arrays, ptrs and derefs ..."
    testDesc = "correctly typechecks passing arrays, ptrs and derefs"
    path = "./test/tests-cases/typechecking/arrays-ptrs-derefs-failures.txt"
    test = \expr -> isLeft (analyseStmtStateful expr testEnv) `shouldBe` True

testEnv :: (Env Semant.Type.Type)
testEnv =
  Env
    { funcs = Map.empty,
      structs = Map.empty,
      scopes = Map.fromList [(rootScopeId, rootScope)],
      currentScopeId = rootScopeId,
      bindingLoc = Toplevel
    }
rootScope :: (Scope Semant.Type.Type)
rootScope =
  Scope
    { id = rootScopeId,
      parentId = Nothing,
      symbolTable = Map.fromList [
        ("ptr", Scalar (PrimitiveType Int 1)),
        ("ptr2",  Scalar (PrimitiveType Int 2)),
        ("arr", Array (PrimitiveType Int 0) [10]),
        ("arr2", Array (PrimitiveType Int 0) [10, 20]),
        ("bag", Array (PrimitiveType Int 3) [10, 20]),
        ("x", Scalar (PrimitiveType Int 0))
      ]
    }
module Typechecker.ArraysPtrsDerefsSpec where

import Test.Hspec

-- typechecksStatefulExpressionsSpec :: Spec
-- typechecksStatefulExpressionsSpec = getSpec specDesc testDesc path test
--   where
--     specDesc = "typechecks literal expressions..."
--     testDesc = "correctly typechecks failing literal expressions"
--     path = "./test/tests-cases/typechecking/stateful-expressions-passes.txt"
--     test = \expr -> isRight (analyseExprStateful' expr testEnv) `shouldBe` True

--     testEnv :: Env
--     testEnv =
--       Env
--         { funcs = Map.empty,
--           structs = Map.empty,
--           scopes = Map.fromList [(rootScopeId, rootScope)],
--           currentScopeId = rootScopeId,
--           bindingLoc = Toplevel
--         }
--     rootScope :: Scope
--     rootScope =
--       Scope
--         { id = rootScopeId,
--           parentId = Nothing,
--           symbolTable = Map.fromList [("a", ("a", Scalar (PrimitiveType Int 0)))]
--         }

module ScopingSpec (scopingSpec) where

import Control.Monad.State (Monad (return), MonadTrans (lift), StateT (runStateT), gets, runState)
import qualified Data.Map as Map (size)
import Lexer.Lexeme
import Parser.Ast
import Semant.Env hiding (defineFunc, defineStruct, lookupFunc, lookupStruct)
import Semant.Scope hiding (defineVar, lookup)
import Semant.Semant
  ( Semant,
    currentScope,
    defineFunc,
    defineStruct,
    defineVar,
    enterScope,
    exitScope,
    getEmptyEnv,
    lookupFunc,
    lookupStruct,
    lookupVar,
    runSemant,
  )
import Semant.Type (Type (Scalar))
import qualified Semant.Type as SemantType
import Test.Hspec
import Prelude hiding (id, lookup)

enterFromRoot :: Semant Bool
enterFromRoot = do
  enterScope
  scope <- currentScope
  return (id scope == 1)

resolveClosest :: Semant [Maybe SemantType.Type]
resolveClosest = do
  defineVar ("x", Scalar (PrimitiveType Int 0))
  enterScope
  defineVar ("x", Scalar (PrimitiveType Double 0))
  enterScope
  defineVar ("x", Scalar (PrimitiveType Char 0))
  enterScope
  resolve1 <- lookupVar "x"
  exitScope
  resolve2 <- lookupVar "x"
  exitScope
  resolve3 <- lookupVar "x"
  exitScope
  resolve4 <- lookupVar "x"
  return [resolve1, resolve2, resolve3, resolve4]

resolveDeep :: Semant (Maybe SemantType.Type, Int)
resolveDeep = do
  defineVar ("x", Scalar (PrimitiveType Int 0))
  enterScope
  enterScope
  enterScope
  enterScope
  enterScope
  x <- lookupVar "x"
  y <- gets (Map.size . scopes)
  return (x, y)

resolveDeep2 :: Semant (Maybe SemantType.Type, Int)
resolveDeep2 = do
  defineVar ("x", Scalar (PrimitiveType Int 0))
  enterScope
  enterScope
  enterScope
  enterScope
  enterScope
  x <- lookupVar "y"
  y <- gets (Map.size . scopes)
  return (x, y)

findFuncFromRoot :: Semant (Maybe Semant.Env.FuncSignature)
findFuncFromRoot = do
  defineFunc
    ( FuncSignature
        { returnType = Scalar (PrimitiveType Char 0),
          funcName = "main",
          formals = []
        }
    )
  lookupFunc "main"

findFuncFromSomewhere :: Semant (Maybe Semant.Env.FuncSignature)
findFuncFromSomewhere = do
  defineFunc
    ( FuncSignature
        { returnType = Scalar (PrimitiveType Char 0),
          funcName = "main",
          formals = []
        }
    )
  enterScope
  enterScope
  lookupFunc "main"

findFuncWithVarName :: Semant (Maybe Semant.Env.FuncSignature)
findFuncWithVarName = do
  defineFunc
    ( FuncSignature
        { returnType = Scalar (PrimitiveType Char 0),
          funcName = "main",
          formals = []
        }
    )
  enterScope
  defineVar ("main", Scalar (PrimitiveType Int 0))
  enterScope
  lookupFunc "main"

dontFindFunc :: Semant (Maybe Semant.Env.FuncSignature)
dontFindFunc = do
  defineFunc
    ( FuncSignature
        { returnType = Scalar (PrimitiveType Char 0),
          funcName = "main",
          formals = []
        }
    )
  enterScope
  enterScope
  lookupFunc "smain"

findStructFromRoot :: Semant (Maybe Semant.Env.StructSignature)
findStructFromRoot = do
  defineStruct StructSignature {structName = "a", fields = []}
  lookupStruct "a"

findStructFromSomewhere :: Semant (Maybe Semant.Env.StructSignature)
findStructFromSomewhere = do
  defineStruct StructSignature {structName = "a", fields = []}
  enterScope
  enterScope
  lookupStruct "a"

findStructWithVarName :: Semant (Maybe Semant.Env.StructSignature)
findStructWithVarName = do
  defineStruct StructSignature {structName = "a", fields = []}
  enterScope
  defineVar ("a", Scalar (PrimitiveType Int 0))
  enterScope
  lookupStruct "a"

findStructWithVarAndFuncName :: Semant (Maybe Semant.Env.StructSignature)
findStructWithVarAndFuncName = do
  defineStruct StructSignature {structName = "a", fields = []}
  defineFunc
    ( FuncSignature
        { returnType = Scalar (PrimitiveType Char 0),
          funcName = "a",
          formals = []
        }
    )
  enterScope
  defineVar ("a", Scalar (PrimitiveType Int 0))
  enterScope
  lookupStruct "a"

dontFindStruct :: Semant (Maybe Semant.Env.FuncSignature)
dontFindStruct = do
  defineStruct StructSignature {structName = "a", fields = []}
  enterScope
  enterScope
  lookupFunc "a_struct"

scopingSpec :: Spec
scopingSpec = do
  describe "lexical scoping resolution tests..." $ do
    it "should enterScope from root" $ do
      let result = runSemant enterFromRoot getEmptyEnv
      result `shouldBe` True
    it "should resolve closest" $
      do
        let result = runSemant resolveClosest getEmptyEnv
        result
          `shouldBe` [ Just (Scalar (PrimitiveType Char 0)),
                       Just (Scalar (PrimitiveType Char 0)),
                       Just (Scalar (PrimitiveType Double 0)),
                       Just (Scalar (PrimitiveType Int 0))
                     ]
    it "should resolve deep" $ do
      let result = runSemant resolveDeep getEmptyEnv
      result `shouldBe` (Just (Scalar (PrimitiveType Int 0)), 6)
    it "should resolve deep2" $
      do
        let result = runSemant resolveDeep2 getEmptyEnv
        result
          `shouldBe` (Nothing, 6)
    it "should find func from root" $ do
      let result = runSemant findFuncFromRoot getEmptyEnv
      result
        `shouldBe` Just
          ( FuncSignature
              { returnType = Scalar (PrimitiveType Char 0),
                funcName = "main",
                formals = []
              }
          )
    it "should find func from not-root" $ do
      let result = runSemant findFuncFromSomewhere getEmptyEnv
      result
        `shouldBe` Just
          ( FuncSignature
              { returnType = Scalar (PrimitiveType Char 0),
                funcName = "main",
                formals = []
              }
          )
    it "should find func instead of var" $ do
      let result = runSemant findFuncWithVarName getEmptyEnv
      result
        `shouldBe` Just
          ( FuncSignature
              { returnType = Scalar (PrimitiveType Char 0),
                funcName = "main",
                formals = []
              }
          )
    it "should not find func" $ do
      let result = runSemant dontFindFunc getEmptyEnv
      result `shouldBe` Nothing
    it "should find struct from root" $ do
      let result = runSemant findStructFromRoot getEmptyEnv
      result
        `shouldBe` Just (StructSignature {structName = "a", fields = []})
    it "should find struct from not-root" $ do
      let result = runSemant findStructFromSomewhere getEmptyEnv
      result
        `shouldBe` Just (StructSignature {structName = "a", fields = []})
    it "should find struct instead of var" $ do
      let result = runSemant findStructWithVarName getEmptyEnv
      result
        `shouldBe` Just (StructSignature {structName = "a", fields = []})
    it "should find struct instead of var and func" $ do
      let result = runSemant findStructWithVarAndFuncName getEmptyEnv
      result
        `shouldBe` Just (StructSignature {structName = "a", fields = []})

    it "should not find struct" $ do
      let result = runSemant dontFindFunc getEmptyEnv
      result `shouldBe` Nothing
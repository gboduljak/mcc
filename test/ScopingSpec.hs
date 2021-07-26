module ScopingSpec (scopingSpec) where

import Control.Monad.State (Monad (return), MonadTrans (lift), StateT (runStateT), gets, runState)
import qualified Data.Map as Map (size)
import Lexer.Lexeme
import Parser.Ast
import Semant.Env
import Semant.Scope hiding (extend, lookup)
import Semant.Semant (Semant, current, enter, exit, extend, getEmptyEnv, lookup, runSemant)
import Semant.Type (Type (Scalar))
import qualified Semant.Type as SemantType
import Test.Hspec
import Prelude hiding (id, lookup)

enterFromRoot :: Semant Bool
enterFromRoot = do
  enter
  scope <- current
  return (id scope == 1)

resolveClosest :: Semant [Maybe SemantType.Type]
resolveClosest = do
  extend ("x", Scalar (PrimitiveType Int 0))
  enter
  extend ("x", Scalar (PrimitiveType Double 0))
  enter
  extend ("x", Scalar (PrimitiveType Char 0))
  enter
  resolve1 <- lookup "x"
  exit
  resolve2 <- lookup "x"
  exit
  resolve3 <- lookup "x"
  exit
  resolve4 <- lookup "x"
  return [resolve1, resolve2, resolve3, resolve4]

resolveDeep :: Semant (Maybe SemantType.Type, Int)
resolveDeep = do
  extend ("x", Scalar (PrimitiveType Int 0))
  enter
  enter
  enter
  enter
  enter
  x <- lookup "x"
  y <- gets (Map.size . scopes)
  return (x, y)

resolveDeep2 :: Semant (Maybe SemantType.Type, Int)
resolveDeep2 = do
  extend ("x", Scalar (PrimitiveType Int 0))
  enter
  enter
  enter
  enter
  enter
  x <- lookup "y"
  y <- gets (Map.size . scopes)
  return (x, y)

scopingSpec :: Spec
scopingSpec = do
  describe "lexical scoping resolution tests..." $ do
    it "should enter from root" $ do
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
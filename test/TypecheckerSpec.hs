module TypecheckerSpec where

import Data.Either
import Data.Foldable (traverse_)
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import qualified Lexer.Combinator.Lexer as CombinatorLexer (lex')
import Lexer.Lexeme (Lexeme)
import Lexer.Token (Token (lexeme))
import qualified Parser.Ast as Ast
import Parser.AstPrettyPrinter
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import qualified Parser.Pratt.Parser as PrattParser (parseExprs)
import Semant.Ast.SemantAst as SAst
import Semant.SemanticAnalyser (analyseExpr')
import System.Console.Pretty (supportsPretty)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension)
import Test.Hspec (Spec, SpecWith, describe, expectationFailure, it, shouldBe)
import TestCases
  ( dynamicProgrammingPrograms,
    miniPrograms,
    sortingPrograms,
  )
import Prelude hiding (lex)

typechecksLiteralExpressionsSpec :: Spec
typechecksLiteralExpressionsSpec =
  describe "typechecks literal expressions ..." $ do
    it "correctly typechecks literal expressions" $ do
      exprsInput <- readFile "./test/tests-cases/typechecking/literal-expressions-passes.txt"
      let tokens = lex "" exprsInput
      case PrattParser.parseExprs "" tokens of
        (Left errorBundle) -> do
          expectationFailure "with pratt, expected successfull parse of exprs"
        (Right exprs) -> do
          traverse_
            ( \expr -> do
                putStrLn $ "   typechecking " ++ prettyPrintExpr expr ++ " ..."
                (isRight . analyseExpr') expr `shouldBe` True
            )
            exprs

prettyPrintExpr :: Ast.Expr -> String
prettyPrintExpr = renderString . layoutSmart defaultLayoutOptions . pretty

printParseErrors errors input = do
  pretty <- supportsPretty
  let errorMessages = prettyPrintErrors errors (pack input) pretty
   in do
        putStrLn errorMessages

lex :: String -> String -> [Token]
lex file input = case result of
  (Left errors) -> []
  (Right tokens) -> tokens
  where
    result = CombinatorLexer.lex' file input
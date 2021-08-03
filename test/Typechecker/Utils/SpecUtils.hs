module Typechecker.Utils.SpecUtils where

import Data.Foldable
import Data.Text hiding (pretty)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import qualified Lexer.Combinator.Lexer as CombinatorLexer
import Lexer.Token
import qualified Parser.Ast as Ast
import Parser.AstPrettyPrinter
import Parser.Errors.PrettyPrinter
import qualified Parser.Pratt.Parser as PrattParser
import System.Console.Pretty
import Test.Hspec
import Prelude hiding (lex)

getExprSpec :: String -> String -> FilePath -> (Ast.Expr -> IO a) -> SpecWith ()
getExprSpec specDesc testDesc path test =
  describe specDesc $ do
    it testDesc $ do
      exprsInput <- readFile path
      let tokens = lex "" exprsInput
      case PrattParser.parseExprs "" tokens of
        (Left errorBundle) -> do
          expectationFailure "with pratt, expected successfull parse of exprs"
        (Right exprs) -> do
          traverse_
            ( \expr -> do
                putStrLn $ "   typechecking " ++ prettyPrintExpr expr ++ " ..."
                test expr
            )
            exprs

getStmtSpec :: String -> String -> FilePath -> (Ast.Statement -> IO a) -> SpecWith ()
getStmtSpec specDesc testDesc path test =
  describe specDesc $ do
    it testDesc $ do
      stmtsInput <- readFile path
      let tokens = lex "" stmtsInput
      case PrattParser.parseStatements "" tokens of
        (Left errorBundle) -> do
          expectationFailure "with pratt, expected successfull parse of exprs"
        (Right stmts) -> do
          traverse_
            ( \stmt -> do
                putStrLn $ "   typechecking " ++ prettyPrintStatement stmt ++ " ..."
                test stmt
            )
            stmts

prettyPrintExpr :: Ast.Expr -> String
prettyPrintExpr = renderString . layoutSmart defaultLayoutOptions . pretty

prettyPrintStatement :: Ast.Statement -> String
prettyPrintStatement = renderString . layoutSmart defaultLayoutOptions . pretty

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
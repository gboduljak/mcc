module Main where

import Data.Foldable
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import qualified Lexer.AdHoc.Lexer as AdHocLex (lex')
import qualified Lexer.Combinator.Lexer as CombinatorLex (lex')
import qualified Lexer.Generator.Lexer as GeneratedLex (lex')
import Lexer.Lexeme
import Lexer.Token
import Parser.Ast
import Parser.AstPrettyPrinter
import Parser.AstVisualiser
import qualified Parser.Generated.Parser as GeneratedParser (parse)
import Preprocessor.IncludesPreprocessor (preprocess)
import Preprocessor.IncludesVisualiser (draw)
import Prettyprinter
import Prettyprinter.Render.String
import System.Environment (getArgs)
import Text.Megaparsec (ParseErrorBundle (ParseErrorBundle), errorBundlePretty)
import Prelude hiding (lex)

main :: IO ()
main = do
  args <- getArgs
  ((result, graph), _) <- preprocess args

  putStrLn "Constructing dependency graph ... "
  putStrLn "Dependency graph is : "
  putStrLn (draw graph)
  putStrLn "Resolving compilation order ..."
  case result of
    (Left errors) -> do
      putStrLn "Compilation halted due to"
      print errors
    (Right order) -> do
      putStrLn "Compilation order resolved ... 💪"
      putStrLn ("Compilation order is : " ++ show order)
      putStrLn ""
      traverse_ compileFile order

compileFile :: String -> IO ()
compileFile file = do
  putStrLn ("Processing " ++ file ++ "...")
  input <- readFile file
  putStrLn "Combinator Lexer..."
  let combResult = CombinatorLex.lex' file input
  case combResult of
    (Left errors) -> (putStrLn . errorBundlePretty) errors
    (Right tokens) -> displayTokens tokens
  putStrLn "AdHoc Lexer..."
  let adHocResult = AdHocLex.lex' file input
  case adHocResult of
    (Left errors) -> print errors
    (Right tokens) -> displayTokens tokens
  putStrLn "Generated Lexer..."
  let alexLexemes = GeneratedLex.lex' input
  displayLexemes alexLexemes

  putStrLn ("Parsing " ++ file ++ " ...")
  let ast = GeneratedParser.parse alexLexemes
  putStrLn ("Parsed " ++ file ++ " ...")
  writeFile (file ++ ".ast.raw.txt") (show ast)
  writeFile (file ++ ".ast.txt") (prettyPrintAst ast)
  writeFile (file ++ ".ast.dot") (visualiseAst ast)

displayTokens :: [Token] -> IO ()
displayTokens = displayLexemes . map lexeme

displayLexemes :: [Lexeme] -> IO ()
displayLexemes = putStrLn . renderString . layoutCompact . concatWith (surround space) . map pretty

prettyPrintAst :: Program -> String
prettyPrintAst = renderString . layoutSmart defaultLayoutOptions . pretty
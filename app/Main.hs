module Main where

import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import qualified Lexer.AdHoc.Lexer as AdHocLex (lex')
import qualified Lexer.Combinator.Lexer as CombinatorLex (lex')
import qualified Lexer.Generator.Lexer as GeneratedLex (lex)
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

  case result of
    (Left errors) -> do
      putStrLn "Compilation halted due to"
      print errors
    (Right order) -> do
      putStrLn ("Compilation order is : " ++ show order)
      let fileToRead = last order
      input <- readFile fileToRead
      let lexRes = CombinatorLex.lex' fileToRead input
      case lexRes of
        (Left errors) -> (putStrLn . errorBundlePretty) errors
        (Right lexemes) -> putStrLn $ renderString $ layoutCompact $ concatWith (surround space) [pretty lexeme | lexeme <- lexemes]
      let lexRes2 = AdHocLex.lex' fileToRead input
      case lexRes2 of
        (Left errors2) -> print errors2
        (Right tokens) -> do
          putStrLn $ renderString $ layoutCompact $ concatWith (surround space) [pretty tok | tok <- tokens]
      let lexemes3 = GeneratedLex.lex input
      putStrLn $ renderString $ layoutCompact $ concatWith (surround space) [pretty lexeme | lexeme <- lexemes3]
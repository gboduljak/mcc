module Main where

import Data.Foldable
import Data.Maybe (fromJust, isJust)
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import qualified Lexer.AdHoc.Lexer as AdHocLex (lex')
import qualified Lexer.Combinator.Lexer as CombinatorLex (lex')
import qualified Lexer.Generator.Lexer as GeneratedLex (lex')
import Lexer.Lexeme
import Lexer.Token
import Parser.Ast
import qualified Parser.Ast as Ast
import Parser.AstPrettyPrinter
import Parser.AstVisualiser
import qualified Parser.Combinator.Naive.Parser as CombinatorParser (parse)
import qualified Parser.Combinator.Predictive.Parser as PredictiveCombinatorParser (parse)
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import qualified Parser.Generated.Parser as GeneratedParser (parse)
import qualified Parser.Pratt.Parser as PrattParser (parse)
import Preprocessor.IncludesPreprocessor (preprocess)
import Preprocessor.IncludesVisualiser (draw)
import Prettyprinter
import Prettyprinter.Render.String
import Semant.Ast.SemantAst (SProgram)
import Semant.Ast.SemantAstVisualiser (visualiseSemantAst)
import Semant.Env (Env)
import Semant.Semant (getBaseEnv)
import Semant.Exports (analyseProgs, prettyPrintSemantError)
import System.Console.Pretty
import System.Environment (getArgs)
import Text.Megaparsec (ParseErrorBundle (ParseErrorBundle))
import Prelude hiding (lex)
import LLVM.Pretty (ppllvm)
import Codegen.Compiler (compile)
import Data.Text.Lazy (unpack)

main :: IO ()
main = do
  fancyTerm <- supportsPretty
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
      maybeProgs <- mapM parseFile order
      if and [isJust prog | (prog, _) <- maybeProgs]
        then do
          let progs = [(file, input, tokens, fromJust prog) | (prog, (file, input, tokens)) <- maybeProgs]
          case analyseProgs progs getBaseEnv of
            (Left (errors, input)) -> do
              let errorMessages = prettyPrintErrors errors (pack input) fancyTerm
              putStrLn errorMessages
            (Right trees) -> do
              traverse_
                ( \(file, tree) -> do
                    writeFile (file ++ ".sast.dot") (visualiseSemantAst tree)
                    print  (compile file tree)
                    putStrLn $ unpack $ ppllvm (compile file tree)
                )
                (zip order trees)
        else do
          return ()
      -- case analyseProgs maybeProgs of

      return ()

parseFile :: String -> IO (Maybe Program, (String, String, [Token]))
parseFile file = do
  supportsFancyTerminal <- supportsPretty

  putStrLn ("Processing " ++ file ++ "...")
  input <- readFile file
  putStrLn "Combinator Lexer..."
  let combResult = CombinatorLex.lex' file input
  case combResult of
    (Left errors) -> do
      let errorMessages = prettyPrintErrors errors (pack input) supportsFancyTerminal
       in do
            putStrLn errorMessages
            return (Nothing, (file, input, []))
    (Right tokens) -> do
      displayTokens tokens
      putStrLn "Combinator Parser..."
      putStrLn ("Parsing " ++ file ++ " using combinator ...")
      putStrLn ("Parsed " ++ file ++ " ...")
      case PredictiveCombinatorParser.parse input tokens of
        (Left errors) -> do
          let errorMessages = prettyPrintErrors errors (pack input) supportsFancyTerminal
           in do
                putStrLn errorMessages
        (Right ast) -> do
          writeFile (file ++ ".ast.comb.raw.txt") (show ast)
          writeFile (file ++ ".ast.comb.txt") (prettyPrintAst ast)
          writeFile (file ++ ".ast.comb.dot") (visualiseAst ast)
      putStrLn "Pratt Parser..."
      putStrLn ("Parsing " ++ file ++ " using Pratt ...")
      putStrLn ("Parsed " ++ file ++ " ...")
      case PrattParser.parse file tokens of
        (Left errors) -> do
          let errorMessages = prettyPrintErrors errors (pack input) supportsFancyTerminal
           in do
                putStrLn errorMessages
                return (Nothing, (file, input, []))
        (Right ast) -> do
          writeFile (file ++ ".ast.pratt.raw.txt") (show ast)
          writeFile (file ++ ".ast.pratt.txt") (prettyPrintAst ast)
          writeFile (file ++ ".ast.pratt.dot") (visualiseAst ast)
          return (Just ast, (file, input, tokens))

-- putStrLn "AdHoc Lexer..."
-- let adHocResult = AdHocLex.lex' file input
-- case adHocResult of
--   (Left errors) -> print errors
--   (Right tokens) -> displayTokens tokens
-- putStrLn "Generated Lexer..."
-- let alexLexemes = GeneratedLex.lex' input
-- displayLexemes alexLexemes

-- putStrLn "Generated Parser..."
-- putStrLn ("Parsing " ++ file ++ " using generator ...")
-- let ast = GeneratedParser.parse alexLexemes
-- putStrLn ("Parsed " ++ file ++ " ...")
-- writeFile (file ++ ".ast.gen.raw.txt") (show ast)
-- writeFile (file ++ ".ast.gen.txt") (prettyPrintAst ast)
-- writeFile (file ++ ".ast.gen.dot") (visualiseAst ast)

displayTokens :: [Token] -> IO ()
displayTokens = displayLexemes . map lexeme

displayLexemes :: [Lexeme] -> IO ()
displayLexemes = putStrLn . renderString . layoutCompact . concatWith (surround space) . map pretty

prettyPrintAst :: Program -> String
prettyPrintAst = renderString . layoutSmart defaultLayoutOptions . pretty
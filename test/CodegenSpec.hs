{-# LANGUAGE BlockArguments #-}

module CodegenSpec where

import Data.Either
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Text.Lazy (unpack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import GHC.IO.Buffer (checkBuffer)
import GHC.Read (readField)
import qualified Lexer.Combinator.Lexer as CombinatorLexer (lex')
import Lexer.Lexeme (BuiltinType (..), Lexeme)
import Lexer.Token (Token (lexeme))
import Parser.Ast as Ast
import Parser.AstPrettyPrinter
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import qualified Parser.Pratt.Parser as PrattParser (parse)
import Preprocessor.IncludesPreprocessor (preprocess)
import Semant.Ast.SemantAst as SAst hiding (funcs, structs)
import Semant.Env (Env (bindingLoc))
import Semant.Errors.SemantError (BindingLoc (..), SemantError)
import SymbolTable.Scope (Scope (..), rootScopeId, symbolTable)
import Semant.Semant (getBaseEnv)
import Semant.Exports (analyseExpr, analyseExprStateful, analyseProg, analyseProgs)
import Semant.ProgramBundle.Bundler (bundle)
import Semant.Type
import System.Console.Pretty (supportsPretty)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, replaceExtension)
import System.Process ( callCommand )
import Test.Hspec (Spec, SpecWith, describe, expectationFailure, it, shouldBe)
import TestCases
  ( dynamicProgrammingPrograms,
    miniPrograms,
    sortingPrograms,
    jmoragPrograms,
    gabrijelPrograms
  )
import Prelude hiding (id, lex)
import Codegen.Compiler (compile)
import LLVM.Pretty (ppllvm)
import Data.List


jmoragProgramsSpec :: Spec
jmoragProgramsSpec = do
  describe "correctly compiles programs taken from https://github.com/jmorag/mcc/ ..." $ do
    mapM_ codegenProgram jmoragPrograms

gabrijelProgramsSpec :: Spec
gabrijelProgramsSpec = do
  describe "correctly compiles Gabrijel's interesting programs..." $ do
    mapM_ codegenProgram gabrijelPrograms

codegenProgram :: String -> SpecWith ()
codegenProgram folder = it ("compiles program " ++ folder ++ "...") $ do
  supportsFancyErrors <- supportsPretty
  files <- getDirectoryContents folder
  let headers = [file | file <- files, takeExtension file == ".h"]
      sources = [file | file <- files, takeExtension file == ".c"]
      order = headers ++ sources
  ast <-
    mapM
      ( \file -> do
          input <- readFile (folder ++ file)
          let tokens = lexCombinator "" $ input
          let ast = parse tokens
          return (file, input, tokens, ast)
      )
      order
  case analyseProgs ast getBaseEnv of
    (Left (errors, input)) -> do
      expectationFailure ("expected successfull semantic analysis of " ++ show folder)
    (Right programs) -> do
      let program = bundle programs
      let llvm = unpack . ppllvm $ compile folder program
      let llvmFile = folder ++ "compiled.ll"
      let input = folder ++ "input.in"
      let expectedOutput = folder ++ "expected-output.txt"
      let actualOutput = folder ++ "actual-output.txt"
      let mccExecutable = folder ++ "compiled.out"
      let clangExecutable = folder ++ "compiled-clang.out"
      let clangSources = [folder ++ source | source <- sources]
      let execMccExecutableCmd = mccExecutable ++ " < " ++ input ++ " > " ++ actualOutput
      let execClangExecutableCmd = clangExecutable ++ " < " ++ input ++ " > " ++ expectedOutput
      
      writeFile llvmFile llvm
      
      callCommand ("clang -Wno-everything " ++ llvmFile ++ " -o " ++ mccExecutable)
      callCommand ("clang -Wno-everything " ++ unwords clangSources ++ " -o " ++ clangExecutable)
      callCommand execMccExecutableCmd
      callCommand execClangExecutableCmd

      actualOutputFile <- readFile actualOutput
      expectedOutputFile <- readFile expectedOutput

      actualOutputFile `shouldBe` expectedOutputFile


prettyPrintSemantError :: SemantError -> String
prettyPrintSemantError = renderString . layoutSmart defaultLayoutOptions . pretty

parse :: [Token] -> Ast.Program
parse tokens = case PrattParser.parse "" tokens of
  (Left errors) -> error "unexpected parse error"
  (Right ast) -> ast

lexCombinator :: String -> String -> [Token]
lexCombinator file input = case result of
  (Left errors) -> []
  (Right tokens) -> tokens
  where
    result = CombinatorLexer.lex' file input
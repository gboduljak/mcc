{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Interface.Exports where

import Interface.Options
import Options.Applicative
import Interface.OptionsParser (options)
import Data.Void
import System.Directory (doesFileExist, doesDirectoryExist, getCurrentDirectory, createDirectory)
import Lexer.Combinator.Lexer (lex')
import Prelude hiding (id)
import System.Console.Pretty
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Lexer.Token (Token (lexeme))
import Control.Monad.Trans (lift, MonadIO (liftIO))
import System.FilePath ((</>), replaceExtension, takeFileName, takeExtension)
import Lexer.Lexeme
import Data.Text.Prettyprint.Doc ( pretty, surround, space, defaultLayoutOptions, layoutSmart )
import Prettyprinter (concatWith, layoutCompact)
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import Parser.Ast
import Preprocessor.IncludesPreprocessor (preprocess)
import Preprocessor.IncludesVisualiser (draw)
import qualified Parser.Pratt.Parser as PrattParser (parse)
import qualified Parser.Combinator.Naive.Parser as NaiveCombinatorParser (parse)
import qualified Parser.Combinator.Predictive.Parser as PredictiveCombinatorParser (parse)
import Parser.AstVisualiser (visualiseAst)
import Parser.AstPrettyPrinter
import Semant.Ast.SemantAst
import Semant.Exports (analyseProgs)
import Semant.Semant (getBaseEnv)
import Semant.Ast.SemantAstVisualiser (visualiseSemantAst)
import Semant.ProgramBundle.Bundler
import Codegen.Compiler (compileBundle, compile)
import LLVM.Pretty
import Data.Text (pack)
import Data.Text.Lazy (unpack)
import Data.Text (split)

mcc :: String
mcc = "\
  \,--.   ,--. ,-----. ,-----. \
  \|   `.'   |'  .--./'  .--./ \
  \|  |'.'|  ||  |    |  |     \
  \|  |   |  |'  '--'\'  '--'  \ 
  \`--'   `--' `-----' `-----' \\"

entry :: IO ()
entry = do
  options <- execParser (options `withInfo` infoString)

  currentDir <- liftIO getCurrentDirectory
  let outFolder = currentDir  </> outputFolder options
  outFolderExists <- liftIO $ doesDirectoryExist outFolder
  unless outFolderExists $ liftIO $ createDirectory outFolder

  allSourcesExist <- ensureAllExist (sourceFiles options)
  when allSourcesExist (void $ runMaybeT $ runOpts options)

 where
  withInfo opts desc = info (helper <*> opts) $ progDesc desc
  infoString
    = "Run mcc on the given list of files. \
       \Passing no flags will attempt to compile the program and emit LLVM IR to ./compiled.ll ."

runOpts :: Options -> MaybeT IO ()
runOpts opts@Options{..} = do
  order <- performPreprocess opts
  case task of
    Lex -> void $ performLex opts order
    Parse -> do
      lexResults <- performLex opts order
      parseResults <- performParse opts lexResults
      return ()
    Semant semantOut -> do
       lexResults <- performLex opts order
       parseResults <- performParse opts lexResults
       semantResults <- performSemant opts parseResults semantOut
       return ()
    LLVM semantOut llvmOut -> do
       lexResults <- performLex opts order
       parseResults <- performParse opts lexResults
       semantResults <- performSemant opts parseResults semantOut
       performCodegen opts semantResults llvmOut
       return ()

baseOutDir :: MonadIO m => Options -> m FilePath
baseOutDir options = do
  currentDir <- liftIO getCurrentDirectory
  return (currentDir </> outputFolder options)

ensureAllExist :: [FilePath] -> IO Bool
ensureAllExist files = and <$> mapM (
  \file -> do
    exists <- doesFileExist file
    if not exists
      then do putStrLn ("The file " ++ file ++ " does not exist.") >> return exists
      else do return exists
  ) files

performPreprocess :: Options -> MaybeT IO [FilePath]
performPreprocess opts@Options{..} = do
  lift $ putStrLn "Constructing dependency graph ...💡 "
  ((result, graph), _) <- lift $ preprocess sourceFiles
  lift $ putStrLn "Resolving compilation order ... 🔍 "

  if outputDependencyGraph
    then do
      outDir <- baseOutDir opts
      let graphFilePath = outDir </> "compilation-order.dot"
      let graphFileContents = draw graph
      lift $ writeFile graphFilePath graphFileContents
    else mzero

  case result of
    (Left errors) -> do
      lift $ putStrLn "Compilation halted due to: "
      mapM_ (lift . print) errors
      mzero
    (Right processOrder) -> do
      lift $ putStrLn ("Compilation order is : " ++ show processOrder ++ ".")
      return processOrder


performLex :: Options -> [FilePath] -> MaybeT IO [(FilePath, String, [Token])]
performLex opts@Options{..} sourceFilesOrder = do
  supportsFancyTerminal <- lift supportsPretty

  results <- mapM (\fileName -> do
      lift $ putStrLn ("Lexing " ++ fileName ++ "... 💪")
      lift $ putStrLn "Done."
      input <- lift $ readFile fileName
      return (fileName, input, lex' fileName input)
    ) sourceFilesOrder

  let errorBundles = [(fileName, input, errors) | (fileName, input, Left errors) <- results]
  let tokenStreams = [(fileName, input, tokens) | (fileName, input, Right tokens) <- results]

  outDir <- baseOutDir opts
  let lexDir = outDir </> "lex"
  lexDirExists <- lift $ doesDirectoryExist lexDir
  unless lexDirExists $ liftIO $ createDirectory lexDir

  mapM_ (\(fileName, input, tokens) -> do
      let fileName' = takeFileName fileName
      let lexOutName = replaceExtension fileName' ".lex"
      let tokensFile = lexDir </> lexOutName
      let tokensFileContent = displayLexemes . map lexeme $ tokens
      lift $ writeFile (lexDir </> tokensFile) tokensFileContent
      return ()
    ) tokenStreams

  case errorBundles of
    [] -> return tokenStreams
    bundles -> do
      mapM_ (\(fileName, input, bundle) -> do
          let errorsToDisplay = prettyPrintErrors bundle (pack input) supportsFancyTerminal
          lift $ putStrLn errorsToDisplay
        ) bundles
      mzero

performParse :: Options -> [(FilePath, String, [Token])] -> MaybeT IO [(FilePath, String, [Token], Program)]
performParse opts@Options{..} lexResults = do
  supportsFancyTerminal <- lift supportsPretty

  let parse = (
        case parserType of
          Pratt -> PrattParser.parse
          NaiveCombinator -> NaiveCombinatorParser.parse
          PredictiveCombinator -> PredictiveCombinatorParser.parse
        )

  parseResults <- mapM (
        \(fileName, input, tokens) -> do
            lift $ putStrLn ("Parsing " ++ fileName ++ "... 🌴")
            lift $ putStrLn "Done."
            return (fileName, input, tokens, parse fileName tokens)
        ) lexResults

  let errorBundles = [
              (fileName, input, tokens, errors) |
              (fileName, input, tokens, Left errors) <-
              parseResults
            ]
      astBundles = [
              (fileName, input, tokens, ast) |
              (fileName, input, tokens, Right ast) <-
              parseResults
            ]
  outDir <- baseOutDir opts
  let parseDir = outDir </> "parse"
  parseDirExists <- lift $ doesDirectoryExist parseDir
  unless parseDirExists $ liftIO $ createDirectory parseDir
  mapM_ (\(fileName, input, _, ast) -> do
      let fileName' = takeFileName fileName
          fileExt = takeExtension fileName
          astOutName = replaceExtension fileName' ".ast.dot"
          prettyFileName = replaceExtension fileName' (".ast"++ fileExt)
          prettyAst = prettyPrintAst ast
          drawnAst = visualiseAst ast
      lift $ writeFile (parseDir </> prettyFileName) prettyAst
      lift $ writeFile (parseDir </> astOutName) drawnAst
      return ()
    ) astBundles

  case errorBundles of
    [] -> return astBundles
    _ -> do
      mapM_ (\(fileName, input, _, bundle) -> do
          let errorsToDisplay = prettyPrintErrors bundle (pack input) supportsFancyTerminal
          lift $ putStrLn errorsToDisplay
        ) errorBundles
      mzero

performSemant :: Options -> [(FilePath, String, [Token], Program)] -> FilePath -> MaybeT IO SProgram
performSemant opts@Options{..} parseResults semantOutFile = do
  lift $ putStrLn "Performing semantic analysis ... 📚"
  supportsFancyTerminal <- lift supportsPretty
  lift $ putStrLn "Done."
  case analyseProgs parseResults getBaseEnv of
    Left (errorBundle, errorInput) -> do
      let errorsToDisplay = prettyPrintErrors errorBundle (pack errorInput) supportsFancyTerminal
      lift $ putStrLn errorsToDisplay
      mzero
    Right programs -> do
      outDir <- baseOutDir opts
      let semantDir = outDir </> "semant"
      semantDirExists <- lift $ doesDirectoryExist semantDir
      unless semantDirExists $ liftIO $ createDirectory semantDir
      mapM_ (
        \(fileName, sast) -> do
          let fileName' = takeFileName fileName
              sastOutName = replaceExtension fileName' ".sast.dot"
              drawnSast = visualiseSemantAst sast
          lift $ writeFile (semantDir </> sastOutName) drawnSast
          return ()
        )
        (zip (map fileName parseResults) programs)
      let compilationBundle = bundle programs
          bundleSast = visualiseSemantAst compilationBundle
      lift $ writeFile (outDir </> semantOutFile) bundleSast
      return compilationBundle
  where fileName (name, _, _, _) = name

performCodegen :: Options -> SProgram -> FilePath -> MaybeT IO ()
performCodegen opts compilationBundle outFile = do
  lift $ putStrLn "Emitting LLVM ... 🖨️ "
  lift $ putStrLn "Done. 😎 "
  outDir <- baseOutDir opts
  let llvm = unpack $ ppllvm (compile outFile compilationBundle)
  lift $ writeFile (outDir </> outFile) llvm
  return ()

displayLexemes :: [Lexeme] -> String
displayLexemes = renderString . layoutSmart defaultLayoutOptions . concatWith (surround space) . map pretty

prettyPrintAst :: Program -> String
prettyPrintAst = renderString . layoutSmart defaultLayoutOptions . pretty
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Preprocessor.IncludesPreprocessor (preprocess) where

import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), MonadTrans (lift), StateT (StateT, runStateT), gets, modify)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Lexer.Combinator.Lexer (lexIncludes)
import Preprocessor.IncludesGraph (IncludesGraph)
import Preprocessor.PreprocessError (PreprocessError (..))
import Preprocessor.TopologicalOrder
  ( TopologicalOrder,
  )
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

data PreprocessorState = PreprocessorState
  { includesGraph :: IncludesGraph,
    processingFiles :: Set String,
    processedFiles :: Set String,
    includesPath :: [String],
    includesTopologicalOrder :: TopologicalOrder,
    errors :: [PreprocessError]
  }

type Preprocessor a = StateT PreprocessorState IO a

logError :: PreprocessError -> Preprocessor ()
logError error =
  modify
    ( \PreprocessorState
         { includesGraph,
           processingFiles,
           processedFiles,
           errors,
           includesPath,
           includesTopologicalOrder
         } ->
          PreprocessorState
            { includesGraph,
              processingFiles,
              processedFiles,
              errors = errors ++ [error],
              includesPath,
              includesTopologicalOrder
            }
    )

addToWorklist :: String -> Preprocessor ()
addToWorklist file =
  modify
    ( \PreprocessorState
         { includesGraph,
           processingFiles,
           processedFiles,
           errors,
           includesPath,
           includesTopologicalOrder
         } ->
          PreprocessorState
            { includesGraph = includesGraph <> Map.fromList [(file, [])],
              processingFiles,
              processedFiles,
              errors,
              includesPath,
              includesTopologicalOrder
            }
    )

addDependency :: String -> String -> Preprocessor ()
addDependency file include =
  modify
    ( \PreprocessorState
         { includesGraph,
           processingFiles,
           processedFiles,
           errors,
           includesPath,
           includesTopologicalOrder
         } ->
          PreprocessorState
            { includesGraph = Map.insert file (include : fromJust (Map.lookup file includesGraph)) includesGraph,
              processingFiles,
              processedFiles,
              errors,
              includesPath,
              includesTopologicalOrder
            }
    )

markProcessing :: String -> Preprocessor ()
markProcessing file =
  modify
    ( \PreprocessorState
         { includesGraph,
           processingFiles,
           processedFiles,
           errors,
           includesPath,
           includesTopologicalOrder
         } ->
          PreprocessorState
            { includesGraph,
              processingFiles = Set.insert file processingFiles,
              processedFiles,
              errors,
              includesPath = includesPath ++ [file],
              includesTopologicalOrder
            }
    )

markProcessed :: String -> Preprocessor ()
markProcessed file =
  modify
    ( \PreprocessorState
         { includesGraph,
           processingFiles,
           processedFiles,
           errors,
           includesPath,
           includesTopologicalOrder
         } ->
          PreprocessorState
            { includesGraph,
              processingFiles = Set.delete file processingFiles,
              processedFiles = Set.insert file processedFiles,
              errors,
              includesPath,
              includesTopologicalOrder = includesTopologicalOrder ++ [file]
            }
    )

isProcessed :: String -> Preprocessor Bool
isProcessed file = gets (Set.member file . processedFiles)

resetIncludesPath :: Preprocessor ()
resetIncludesPath =
  modify
    ( \PreprocessorState
         { includesGraph,
           processingFiles,
           processedFiles,
           errors,
           includesPath,
           includesTopologicalOrder
         } ->
          PreprocessorState
            { includesGraph,
              processingFiles,
              processedFiles,
              errors,
              includesPath = [],
              includesTopologicalOrder
            }
    )

registerSourceFile :: String -> Preprocessor ()
registerSourceFile file
  | takeExtension file `notElem` [".c", ".h"] = logError (InvalidExtension file)
  | otherwise = do
    exists <- liftIO $ doesFileExist file
    graph <- gets includesGraph
    alreadyRegistered <- gets (isJust . Map.lookup file . includesGraph)
    if not exists
      then logError (NonexistentFile file)
      else do
        if alreadyRegistered
          then return ()
          else do
            addToWorklist file
            input <- liftIO $ readFile file
            let includes = lexIncludes file (pack input)
            case includes of
              (Left errors) -> logError (LexError errors)
              (Right includes) ->
                traverse_
                  ( \includeFile -> do
                      addDependency file includeFile
                      case Map.lookup includeFile graph of
                        Nothing -> do registerSourceFile includeFile
                        _ -> return ()
                  )
                  includes

buildDependencyGraph :: [String] -> Preprocessor (Either [PreprocessError] IncludesGraph)
buildDependencyGraph files = do
  traverse_ registerSourceFile files
  errors <- gets errors
  includeGraph <- gets includesGraph
  if null errors
    then return (Right includeGraph)
    else return (Left errors)

topSort :: [String] -> Preprocessor (Either [PreprocessError] TopologicalOrder)
topSort files = do
  traverse_
    ( \file -> do
        processed <- isProcessed file
        if processed
          then do return ()
          else do
            resetIncludesPath
            topSortDfs file
    )
    files
  errors <- gets errors
  order <- gets includesTopologicalOrder
  case errors of
    [] -> return (Right order)
    errorBundle -> return (Left errorBundle)

topSortDfs :: String -> Preprocessor ()
topSortDfs file = do
  graph <- gets includesGraph
  markProcessing file
  let adjacencyList = fromJust $ Map.lookup file graph
   in do
        traverse_
          ( \dependency -> do
              includesPath <- gets includesPath
              processingFiles <- gets processingFiles
              processedFiles <- gets processedFiles
              if dependency `Set.member` processingFiles
                then do
                  cycle <- extractCycle dependency
                  logError (Cycle cycle)
                else do
                  if dependency `Set.member` processedFiles
                    then do return ()
                    else do topSortDfs dependency
          )
          adjacencyList
          >> markProcessed file
  where
    extractCycle from = do
      includesPath <- gets includesPath
      let cycle = takeWhile (/= from) (reverse includesPath)
       in return (reverse ([from] ++ cycle ++ [from]))

processIncludes :: [String] -> Preprocessor (Either [PreprocessError] TopologicalOrder)
processIncludes files = do
  graphResult <- buildDependencyGraph files
  case graphResult of
    (Right graph) -> topSort files
    (Left errors) -> return (Left errors)

preprocess ::
  [String] ->
  IO
    ( ( Either [PreprocessError] TopologicalOrder,
        IncludesGraph
      ),
      PreprocessorState
    )
preprocess files = runStateT preprocessAction initState
  where
    initState =
      PreprocessorState
        { includesGraph = Map.empty,
          processingFiles = Set.empty,
          processedFiles = Set.empty,
          errors = [],
          includesPath = [],
          includesTopologicalOrder = []
        }
    preprocessAction = do
      result <- processIncludes files
      graph <- gets includesGraph
      return (result, graph)
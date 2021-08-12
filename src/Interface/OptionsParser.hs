module Interface.OptionsParser where

import Options.Applicative
import Options.Applicative.Builder
import Interface.Options (ParserType(..), LexerType (..), Task (Lex, Parse, Semant, LLVM), Options (Options))

options :: Parser Options 
options = Options <$> 
  lexer <*> 
  parser <*> 
  task <*> 
  sourceFiles <*> 
  outFolder <*> 
  outDependencyGraph

sourceFiles :: Parser [FilePath]
sourceFiles = many (argument str (metavar "SOURCE FILES..."))

outFolder :: Parser FilePath
outFolder = strOption (
    short 'o'
      <> long "out"
      <> metavar "FOLDER PATH"
      <> value "build"
      <> help "Compilation output folder."
  )

task :: Parser Task
task = flag' Lex (
    long "lex" <>
    help "Perform only lexical analysis."
  )
  <|> flag' Parse (
    long "parse" <>
    help "Perform lexical and syntactic analysis."
  )
  <|> flag' Semant (
    long "semant" <>
    help "Perform lexical, syntactic and sematic analysis."
  ) <*>
      strOption (long "semant-to" <> help "semantic analysis output filename" <> value "merged-semant-ast.dot")
  <|> flag' LLVM (
      long "llvm" <>
      help "Perform compilation to LLVM IR"
    ) <*>
        strOption (long "semant-to" <> help "semantic analysis output output filename" <> value "merged-semant-ast.dot")
      <*>
        strOption (long "llvm-to" <> help "LLVM output filename" <> value "compiled.ll")
  <|> LLVM 
      <$>
        strOption (long "semant-to" <> help "semantic analysis output output filename" <> value "merged-semant-ast.dot")
      <*>
        strOption (long "llvm-to" <> help "LLVM output filename" <> value "compiled.ll")

parser :: Parser ParserType
parser =
  flag' Pratt (
    long "pratt" <>
    help "Use the Pratt parser (default)."
  )
  <|> flag' PredictiveCombinator (
    long "predictive-combinator" <>
    help "Use the predictive combinator parser."
  )
  <|> flag' NaiveCombinator  (
    long "naive-combinator" <>
    help "Use the naive combinator parser."
  )
  <|> pure Pratt

lexer :: Parser LexerType
lexer =
  flag' Combinator (
    long "combinator" <>
    help "Use the combinator lexer (default)."
  )
  <|> pure Combinator

outDependencyGraph :: Parser Bool 
outDependencyGraph = flag' False (
    long "no-dep-graph" <>
    help "Do not output dependency graph."
  )
  <|> pure True
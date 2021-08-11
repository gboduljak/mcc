module Interface.Options 

where 

data LexerType = Combinator | AdHoc
data ParserType = NaiveCombinator | PredictiveCombinator | Pratt
data Task = Lex | Parse | Semant FilePath | LLVM FilePath

data Options = Options {
  lexerType :: LexerType,
  parserType :: ParserType,
  task :: Task,
  sourceFiles :: [FilePath],
  outputFolder :: FilePath,
  outputDependencyGraph :: Bool
}
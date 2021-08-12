module Interface.Options 

where 

data LexerType = Combinator deriving Show
data ParserType = NaiveCombinator | PredictiveCombinator | Pratt deriving Show
data Task = Lex | Parse | Semant FilePath | LLVM FilePath FilePath deriving Show

data Options = Options {
  lexerType :: LexerType,
  parserType :: ParserType,
  task :: Task,
  sourceFiles :: [FilePath],
  outputFolder :: FilePath,
  outputDependencyGraph :: Bool
} deriving Show
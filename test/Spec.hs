import qualified LexerSpec as LexerSpec (dynamicProgrammingSpec, miniProgramsSpec, sortingSpec, ticTacToeSpec)
import qualified ParserSpec as ParserSpec (dynamicProgrammingSpec, miniProgramsSpec, sortingSpec, ticTacToeSpec)
import qualified ScopingSpec as ScopingSpec (scopingSpec)
import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec LexerSpec.miniProgramsSpec
  hspec LexerSpec.sortingSpec
  hspec LexerSpec.dynamicProgrammingSpec
  hspec LexerSpec.ticTacToeSpec
  hspec ParserSpec.miniProgramsSpec
  hspec ParserSpec.sortingSpec
  hspec ParserSpec.dynamicProgrammingSpec
  hspec ParserSpec.ticTacToeSpec
  hspec ScopingSpec.scopingSpec
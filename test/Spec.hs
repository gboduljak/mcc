import qualified LexerSpec (dynamicProgrammingSpec, miniProgramsSpec, sortingSpec, ticTacToeSpec)
import qualified ParserSpec (dynamicProgrammingSpec, miniProgramsSpec, sortingSpec, ticTacToeSpec)
import qualified ScopingSpec (scopingSpec)
import Test.Hspec (hspec)
import qualified TypecheckerSpec (typechecksLiteralExpressions)

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
  hspec TypecheckerSpec.typechecksLiteralExpressions
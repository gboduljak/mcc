import LexerSpec (dynamicProgrammingSpec, miniProgramsSpec, sortingSpec, ticTacToeSpec)
import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec miniProgramsSpec
  hspec sortingSpec
  hspec dynamicProgrammingSpec
  hspec ticTacToeSpec
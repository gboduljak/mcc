import LexerSpec (dynamicProgrammingSpec, sortingSpec, ticTacToeSpec)
import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec sortingSpec
  hspec dynamicProgrammingSpec
  hspec ticTacToeSpec
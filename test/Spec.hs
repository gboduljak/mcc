import qualified LexerSpec (dynamicProgrammingSpec, miniProgramsSpec, sortingSpec, ticTacToeSpec)
import qualified ParserSpec (dynamicProgrammingSpec, miniProgramsSpec, sortingSpec, ticTacToeSpec)
import qualified ScopingSpec (scopingSpec)
import qualified SemanticAnalyserSpec (dynamicProgrammingSpec, miniProgramsSpec, sortingSpec, ticTacToeSpec)
import Test.Hspec (hspec)
import qualified Typechecker.LiteralExpressionsSpec (typechecksFailingLiteralExpressionsSpec, typechecksLiteralExpressionsSpec, typechecksStatefulExpressionsSpec)
import qualified Typechecker.ArraysPtrsDerefsSpec (typechecksArraysPtrsDerefsPassingSpec, typechecksArraysPtrsDerefsFailingSpec)
import qualified Typechecker.StatementsSpec (statementsPassingSpec)
import qualified CodegenSpec (jmoragProgramsSpec, gabrijelProgramsSpec)
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
  hspec Typechecker.LiteralExpressionsSpec.typechecksLiteralExpressionsSpec
  hspec Typechecker.LiteralExpressionsSpec.typechecksFailingLiteralExpressionsSpec
  hspec Typechecker.LiteralExpressionsSpec.typechecksStatefulExpressionsSpec
  hspec SemanticAnalyserSpec.miniProgramsSpec
  hspec SemanticAnalyserSpec.sortingSpec
  hspec SemanticAnalyserSpec.dynamicProgrammingSpec
  hspec SemanticAnalyserSpec.ticTacToeSpec
  hspec Typechecker.ArraysPtrsDerefsSpec.typechecksArraysPtrsDerefsPassingSpec
  hspec Typechecker.ArraysPtrsDerefsSpec.typechecksArraysPtrsDerefsFailingSpec
  hspec Typechecker.StatementsSpec.statementsPassingSpec
  hspec CodegenSpec.jmoragProgramsSpec
  hspec CodegenSpec.gabrijelProgramsSpec
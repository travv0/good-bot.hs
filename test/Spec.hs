import Test.Hspec
import qualified CalculatorSpec
import qualified TypesSpec

main :: IO ()
main = hspec $ do
    describe "Calculator" CalculatorSpec.spec
    describe "Types" TypesSpec.spec
import RIO
import Spec.Exceptions
import Spec.Textual
import Spec.Util
import Test.Hspec

main :: IO ()
main = hspec $ do
  testExceptions
  testTextual
  testUtil
import RIO
import Spec.Crypto
import Spec.Exceptions
import Spec.Settings
import Spec.Textual
import Spec.Util
import Test.Hspec

main :: IO ()
main = hspec $ do
  testCrypto
  testExceptions
  testTextual
  testUtil
  testSettings
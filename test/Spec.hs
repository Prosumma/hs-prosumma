import RIO
import Spec.AWS.DynamoDB
import Spec.Cache
import Spec.Crypto
import Spec.Exceptions
import Spec.Settings
import Spec.Textual
import Spec.Util
import Test.Hspec

main :: IO ()
main = hspec $ do
  testDynamoDB
  testCache
  testCrypto
  testExceptions
  testTextual
  testUtil
  testSettings
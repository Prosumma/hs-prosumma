import RIO

import Spec.Aeson
import Spec.AWS.DynamoDB
import Spec.Cache
import Spec.Crypto
import Spec.Exceptions
import Spec.Push
import Spec.Textual
import Spec.Types
import Spec.Util
import Test.Hspec

main :: IO ()
main = hspec $ do
  testAeson
  testDynamoDB
  testCache
  testCrypto
  testExceptions
  testPush
  testTextual
  testTypes
  testUtil
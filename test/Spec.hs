import RIO

import Spec.Aeson
import Spec.AWS.DynamoDB
import Spec.Cache
import Spec.Crypto
import Spec.Exceptions
import Spec.Push.APNS
import Spec.SQLite
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
  testAPNS
  testSQLite
  testTextual
  testTypes
  testUtil

module Spec.Crypto (testCrypto) where

import Amazonka
import Prelude (putStrLn)
import Prosumma
import Prosumma.AWS
import Prosumma.Crypto
import RIO
import Test.Hspec

data TestContext = TestContext { tcEnv :: !Env, tcMasterKeyArn :: !Text }

instance HasAWSEnv TestContext where
  getAWSEnv = tcEnv

instance HasMasterKeyArn TestContext where
  getMasterKeyArn = tcMasterKeyArn

testCrypto :: Spec
testCrypto = do
  describe "crypto" $
    it "symmetric cryptography with KMS works" $ do 
      env <- newEnv discover
      masterKeyArn <- envString Nothing "AWS_MASTER_KEY_ARN"
      let context = TestContext env masterKeyArn
      let input = "watusi"
      decrypted <- runRIO context $ do
        encrypted <- encryptMessage input 
        decryptMessage encrypted
      input `shouldBe` decrypted
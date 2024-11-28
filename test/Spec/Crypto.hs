module Spec.Crypto (testCrypto) where

import Amazonka
import Prelude (print)
import Prosumma
import Prosumma.Crypto
import RIO
import Test.Hspec

testCrypto :: Spec
testCrypto = do
  describe "symmetric cryptography" $ do
    it "successfully encrypts and decrypts" $ do
      masterKeyArn <- envString Nothing "AWS_MASTER_KEY_ARN"
      env <- liftIO $ newEnv discover
      bazzle <- runRIO env $ runResourceT $ do
        x <- encryptMessageWithMasterKey masterKeyArn "bazzle"
        liftIO $ print x
        decryptMessage x
      bazzle `shouldBe` "bazzle"
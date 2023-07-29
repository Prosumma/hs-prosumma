{-# LANGUAGE DataKinds, DeriveDataTypeable, FlexibleContexts, TypeApplications #-}

module Prosumma.Crypto (
  decryptMessage,
  encryptMessage,
  encryptMessageWithMasterKey,
  DecryptionException,
  EncryptionException,
  HasMasterKeyArn(..)
) where

import Amazonka
import Amazonka.Data.ByteString
import Amazonka.KMS
import Control.Lens ((?~))
import Crypto.TripleSec hiding (DecryptionException, EncryptionException)
import Data.Generics.Product
import Prosumma.AWS
import Prosumma.Util
import RIO

import qualified RIO.ByteString as BS

keyLength :: Int
keyLength = 184

throwOnTripleSecException :: (MonadUnliftIO m, Exception e) => e -> TripleSecException -> m ByteString
throwOnTripleSecException e _ = throwIO e 

class HasMasterKeyArn a where
  getMasterKeyArn :: a -> Text

data EncryptionException = EncryptionException deriving (Show, Typeable)
instance Exception EncryptionException

encryptMessageWithMasterKey
  :: (MonadUnliftIO m, MonadReader env m, HasAWSEnv env)
  => Text -> ByteString -> m ByteString
encryptMessageWithMasterKey masterKeyArn message = do
  resp <- sendAWS $ newGenerateDataKey masterKeyArn &
    (field @"keySpec") ?~ DataKeySpec_AES_256
  let password = toBS $ resp^.(field @"plaintext")
  let ciphertextBlob = unBase64 $ resp^.(field @"ciphertextBlob")
  liftIO $ 
    flip catch (throwOnTripleSecException EncryptionException) $ 
      BS.append ciphertextBlob <$> encryptIO password message

encryptMessage
  :: (MonadUnliftIO m, MonadReader env m, HasMasterKeyArn env, HasAWSEnv env)
  => ByteString -> m ByteString
encryptMessage = asks getMasterKeyArn >>=> encryptMessageWithMasterKey 

data DecryptionException = DecryptionException deriving (Show, Typeable)
instance Exception DecryptionException

decryptMessage 
  :: ( MonadUnliftIO m, MonadReader env m, MonadThrow m, HasAWSEnv env)
  => ByteString -> m ByteString
decryptMessage message =
  if BS.length message <= keyLength
    then throwM DecryptionException
    else do
      let ciphertextBlob = BS.take keyLength message
      let ciphertext = BS.drop keyLength message
      maybePassword <- sendAWS (newDecrypt ciphertextBlob) <&>
        fmap toBS . view (field @"plaintext")
      case maybePassword of
        Nothing -> throwIO DecryptionException
        Just password -> liftIO $
          flip catch (throwOnTripleSecException DecryptionException) $
            decryptIO password ciphertext

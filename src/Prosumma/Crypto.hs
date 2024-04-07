{-# LANGUAGE FlexibleContexts, TypeApplications #-}

module Prosumma.Crypto (
  CryptoException(..),
  combineMessageWithKeyAndIV,
  decryptMessage,
  encryptMessage,
  encryptMessageWithMasterKey
) where

import Amazonka.Data
import Amazonka.KMS
import Control.Lens ((?~))
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Crypto.Random
import Data.Generics.Product
import Data.ByteArray (convert)
import Data.String.Conversions
import Prosumma.AWS
import Prosumma.Util
import RIO

import qualified RIO.ByteString as ByteString

class HasMasterKeyArn a where
  getMasterKeyArn :: a -> Text

data CryptoException = EncryptionException | DecryptionException deriving Show
instance Exception CryptoException

-- | Length of the enciphered key.
-- When decrypted, it's 32 bytes, which AES256 expects.
keyLength :: Int
keyLength = 184 

ivLength :: Int
ivLength = 16

generateIV :: MonadIO m => m (IV AES256) 
generateIV = do 
  iv <- makeIV <$> liftIO bytes
  case iv of
    Nothing -> throwIO EncryptionException
    Just iv -> return iv
  where
    bytes :: IO ByteString 
    bytes = getRandomBytes ivLength 

combineMessageWithKeyAndIV :: MonadIO m => CryptoException -> ByteString -> IV AES256 -> ByteString -> m ByteString
combineMessageWithKeyAndIV e key iv plaintext = do
  case cipherInit key :: CryptoFailable AES256 of
    CryptoFailed _ -> throwIO e 
    CryptoPassed cipher -> return $ ctrCombine cipher iv plaintext

encryptMessageWithMasterKey
  :: (MonadUnliftIO m, MonadThrow m, MonadReader env m, HasAWSEnv env)
  => Text -> ByteString -> m ByteString
encryptMessageWithMasterKey masterKeyArn plaintext = do 
  resp <- sendAWSThrowOnStatus $ newGenerateDataKey masterKeyArn
    & (field @"keySpec") ?~ DataKeySpec_AES_256 
  let password = unBase64 $ fromSensitive $ resp^.(field @"plaintext")
  let ciphertextBlob = unBase64 $ resp^.(field @"ciphertextBlob")
  iv <- generateIV
  encrypted <- combineMessageWithKeyAndIV EncryptionException password iv plaintext
  return $ ciphertextBlob <> convert iv <> encrypted

encryptMessage
  :: (MonadUnliftIO m, MonadThrow m, MonadReader env m, HasAWSEnv env, HasMasterKeyArn env)
  => ByteString -> m ByteString
encryptMessage = asks getMasterKeyArn >>=> encryptMessageWithMasterKey 

decryptMessage
  :: (MonadUnliftIO m, MonadThrow m, HasAWSEnv env, MonadReader env m)
  => ByteString -> m ByteString
decryptMessage ciphertext = do 
  when (ByteString.length ciphertext <= keyLength) $ throwIO DecryptionException
  let (encryptedKey, rest) = ByteString.splitAt keyLength ciphertext
  resp <- sendAWSThrowOnStatus $ newDecrypt encryptedKey 
  case unBase64 . fromSensitive <$> resp^.(field @"plaintext") of
    Nothing -> throwIO DecryptionException
    Just decryptedKey -> do 
      when (ByteString.length rest <= ivLength) $ throwIO DecryptionException
      let (ivBytes, ciphertext') = ByteString.splitAt ivLength rest
      case makeIV ivBytes of
        Nothing -> throwIO DecryptionException
        Just iv -> combineMessageWithKeyAndIV DecryptionException decryptedKey iv ciphertext'

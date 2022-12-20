{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Spec.Exceptions (
  testExceptions
) where

import Control.Monad.Error.Class
import RIO
import Prosumma.Exceptions
import Prosumma.Logging
import Test.Hspec

newtype ServerError = ServerError Int deriving (Show, Typeable)
instance Exception ServerError

newtype SqlError = SqlError ByteString deriving (Show, Typeable)
instance Exception SqlError

data RandomError = RandomError deriving (Show, Typeable)
instance Exception RandomError

matchByCode :: ByteString -> HelpMatchException SqlError ServerError
matchByCode code = throwWhen $ \(SqlError code') -> code == code'

matchDefaultErrors :: MatchException SomeException SomeException
matchDefaultErrors = matchException $ matchByCode "NOAPP" (ServerError 6) >=> matchByCode "NOUSR" (ServerError 22)

matchRandom :: MatchException RandomError ServerError
matchRandom = const $ throwError $ ServerError 0

isServerError :: Int -> Selector ServerError
isServerError n (ServerError n') = n == n'

isSqlError :: ByteString -> Selector SqlError
isSqlError code (SqlError code') = code == code'

newtype App a = App { runApp :: IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadUnliftIO App where
  withRunInIO action = App $ withRunInIO $ \run -> action (run . runApp)

instance HasLogging App where
  allowLogging = return True 

testExceptions :: Spec
testExceptions = do 
  describe "catchMatch" $ do
    it "should match and rethrow exceptions properly" $
      -- SqlError "NOAPP" is mapped to ServerError 6.
      flip shouldThrow (isServerError 6) $ 
        runApp $ catchMatch matchDefaultErrors $ throwM $ SqlError "NOAPP"
    it "should throw the original exception if nothing was matched" $
      -- SqlError "NADA" isn't mapped to anything. 
      flip shouldThrow (isSqlError "NADA") $
        runApp $ catchMatch matchDefaultErrors $ throwM $ SqlError "NADA"
    it "should handle exceptions of multiple types" $
      -- RandomError is mapped to ServerError 6
      flip shouldThrow (isServerError 0) $
        runApp $ catchMatch (matchException matchRandom <=< matchDefaultErrors) $ throwM RandomError

  describe "matchAll" $ do
    let fallback = matchAll $ ServerError 500
    it "should provide a fallback when last in the chain without a match" $ 
      flip shouldThrow (isServerError 500) $
        -- RandomError isn't mapped to anything, so the fallback runs.
        runApp $ catchMatch (fallback <=< matchDefaultErrors) $ throwM RandomError
    it "should not provide a fallback when last in the chain with a match" $
      flip shouldThrow (isServerError 6) $
        -- "NOAPP" is matched to ServerError 6, so the fallback never runs.
        runApp $ catchMatch (matchDefaultErrors >=> fallback) $ throwM $ SqlError "NOAPP" 
    it "should override everything when executed first in the match chain" $
      flip shouldThrow (isServerError 500) $
        -- The fallback occurs first, so everything else is skipped.
        runApp $ catchMatch (fallback >=> matchDefaultErrors) $ throwM $ SqlError "NOAPP"
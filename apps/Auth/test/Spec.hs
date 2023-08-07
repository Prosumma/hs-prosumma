{-# LANGUAGE DataKinds #-}

import Data.String.Conversions
import Data.UUID hiding (null)
import Data.UUID.V4
import Network.HTTP.Client hiding (port, Proxy)
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as Warp
import Prosumma.Auth.API
import Prosumma.Auth.App
import Prosumma.Auth.Server
import Prosumma.Auth.Types
import Prosumma.Logging
import Prosumma.Servant
import RIO
import qualified RIO.Text as T
import Servant
import Servant.Client hiding (baseUrl, manager)
import Test.Hspec
import Test.HUnit
import Text.Printf

withUserApp :: AppContext -> (Warp.Port -> IO ()) -> IO ()
withUserApp ctx action = do
  Warp.testWithApplication (return $ runApplicationWithDefaultLogging apiProxy api ctx) action

randomEmail :: MonadIO m => m Text
randomEmail = do
  identifier <- liftIO nextRandom
  let email = printf "%s@test.com" (toString identifier) :: String
  return $ convertString email

main :: IO ()
main = do
  ctx <- runRIO newLogger $ withDefaultLogging fromEnvironment
  hspec $ around (withUserApp ctx) $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
    let runClient port action = runClientM action (clientEnv port)
    let validateCode :<|> validatePassword :<|> createUser :<|> addContact :<|> _checkHealth = client (Proxy :: Proxy API)

    describe "createUser" $ do

      it "gets a code even if no password is sent" $ \port -> do
        contact <- randomEmail
        response <- runClient port $
          createUser $ CreateUserRequest "test" Nothing contact
        case response of
          Left e -> assertFailure $ show e
          Right (CreateUserResponse code) -> code `shouldSatisfy` (not . T.null)

      it "gets a code when a password is sent" $ \port -> do
        contact <- randomEmail
        response <- runClient port $
          createUser $ CreateUserRequest "test" (Just "whatever") contact
        case response of
          Left e -> assertFailure $ show e
          Right (CreateUserResponse code) -> code `shouldSatisfy` (not . T.null)

      it "gives 422 if the app does not exist" $ \port -> do
        response <- runClient port $
          createUser $ CreateUserRequest "invalid" Nothing "user@test.com"
        case response of
          Left (FailureResponse _ r) -> responseStatusCode r `shouldBe` status422
          _failed -> assertFailure "expected to get a 422"

      it "gives 422 if the contact is not valid" $ \port -> do
        response <- runClient port $
          createUser $ CreateUserRequest "test" Nothing "x"
        case response of
          Left (FailureResponse _ r) -> responseStatusCode r `shouldBe` status422
          _failed -> assertFailure "expected to get a 422"

    describe "validateCode" $ do

      it "succeeds if the correct code is given" $ \port -> do
        contact <- randomEmail
        Right (CreateUserResponse code) <- runClient port $
          createUser $ CreateUserRequest "test" (Just "foobar") contact
        response <- runClient port $
          validateCode $ ValidateCodeRequest "test" contact code
        case response of
          Left e -> assertFailure $ show e
          Right _response -> return ()

      it "fails if the correct code is not given" $ \port -> do
        contact <- randomEmail
        void $ runClient port $
          createUser $ CreateUserRequest "test" (Just "foobar") contact
        response <- runClient port $
          validateCode $ ValidateCodeRequest "test" contact "xyz"
        case response of
          Left _error -> return ()
          Right _response -> assertFailure "Expected code validation to fail, but it succeeded!"

    describe "validatePassword" $ do

      it "succeeds if the correct password is given" $ \port -> do
        contact <- randomEmail
        void $ runClient port $ createUser $ CreateUserRequest "test" (Just "foobar") contact
        response <- runClient port $ validatePassword $ ValidatePasswordRequest "test" contact "foobar"
        case response of
          Left e -> assertFailure $ show e
          Right _object -> return ()

      it "gives 422 if the app does not exist" $ \port -> do
        contact <- randomEmail
        void $ runClient port $ createUser $ CreateUserRequest "test" (Just "foobar") contact
        response <- runClient port $ validatePassword $ ValidatePasswordRequest "invalid" contact "foobar"
        case response of
          Left (FailureResponse _ r) -> responseStatusCode r `shouldBe` status422
          _otherwise -> assertFailure "Expected HTTP 422"

      it "fails if an incorrect password is given" $ \port -> do
        contact <- randomEmail
        void $ runClient port $ createUser $ CreateUserRequest "test" (Just "watusi") contact
        response <- runClient port $ validatePassword $ ValidatePasswordRequest "test" contact "foobar"
        case response of
          Left _error -> return ()
          Right _object -> assertFailure "Expected password validation to fail, but it succeeded!"

    describe "addContact" $ do

      it "succeeds if the user exists and is active" $ \port -> do
        contact <- randomEmail
        Right (CreateUserResponse code) <- runClient port $ createUser $ CreateUserRequest "test" Nothing contact
        Right (ValidateCodeResponse identifier) <- runClient port $ validateCode $ ValidateCodeRequest "test" contact code
        newContact <- randomEmail
        response <- runClient port $ addContact $ AddContactRequest "test" identifier newContact
        case response of
          Left e -> assertFailure $ show e
          Right (AddContactResponse code) -> code `shouldSatisfy` (not . T.null)

      it "fails if the user does not exist" $ \port -> do
        identifier <- nextRandom
        response <- runClient port $ addContact $ AddContactRequest "test" identifier "foo@bar.com"
        case response of
          Left (FailureResponse _ r) -> responseStatusCode r `shouldBe` status422
          Left e -> assertFailure $ show e
          Right _response -> assertFailure "Expected failure when adding a contact to a user that does not exist, but it succeeded!"

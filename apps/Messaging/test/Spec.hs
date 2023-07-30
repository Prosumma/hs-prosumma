{-# LANGUAGE QuasiQuotes #-}

import Prosumma
import Prosumma.Messaging.Email.Types
import Test.Hspec
import Text.Email.Parser
import Text.Email.QuasiQuotation
import RIO

import qualified RIO.Map as Map
import qualified RIO.Set as Set

newtype TestAppContext a = TestAppContext a 

instance HasEmailTemplates (TestAppContext (Set EmailTemplate)) where
  getEmailTemplates (TestAppContext templates) = templates

emailTemplates :: Set EmailTemplate
emailTemplates = Set.fromList ["any_test_en", "any_test_en-GB", "app_test_en", "app_test_en-GB"]

emailTemplateNotFoundSelector :: EmailTemplateNotFound -> Bool
emailTemplateNotFoundSelector _e = True

instance HasSourceEmails (TestAppContext (Map EmailTemplateKey EmailAddress)) where
  getSourceEmails (TestAppContext emails) = emails

sourceEmails :: Map EmailTemplateKey EmailAddress
sourceEmails = Map.fromList [
    ("app_any", [email|any@app.com|]),
    ("app_code", [email|code@app.com|])
  ] 

sourceEmailNotFoundSelector :: SourceEmailNotFound -> Bool
sourceEmailNotFoundSelector _e = True

main :: IO ()
main = hspec $ do 
  describe "EmailTemplate" $ do
    it "can be converted from valid text with full localization" $ do
      let text = "any_code_en-GB" :: Text
      fromText text `shouldBe` (Just "any_code_en-GB" :: Maybe EmailTemplate)
    it "can be converted from valid text with language only" $ do
      let text = "any_code_en" :: Text
      fromText text `shouldBe` (Just "any_code_en" :: Maybe EmailTemplate)
    it "can be converted from a string" $ do
      "any_code_de-DE" `shouldBe` ("any_code_de-DE" :: EmailTemplate) 
    it "cannot be converted from invalid text" $ do
      let text = "^any_code_eng"
      fromText text `shouldBe` (Nothing :: Maybe EmailTemplate)
  describe "normalizeEmailTemplate" $ do
    let templateContext = TestAppContext emailTemplates
    it "should succeed if the exact template is in the set" $ do
      template <- runRIO templateContext $ normalizeEmailTemplate "app_test_en-GB"
      template `shouldBe` "app_test_en-GB"
    it "should succeed if a matching template without region is found" $ do
      template <- runRIO templateContext $ normalizeEmailTemplate "app_test_en-DE"
      template `shouldBe` "app_test_en"
    it "should succeed if a matching template for unknown app is found" $ do
      template <- runRIO templateContext $ normalizeEmailTemplate "foo_test_en-GB"
      template `shouldBe` "any_test_en-GB"
    it "should provide the root template if no matching app or localization is found" $ do
      template <- runRIO templateContext $ normalizeEmailTemplate "foo_test_de-DE"
      template `shouldBe` "any_test_en"
    it "should throw EmailTemplateNotFound if the template name is not found" $ do
      let action = runRIO templateContext $ normalizeEmailTemplate "foo_bar_de-DE"
      action `shouldThrow` emailTemplateNotFoundSelector
  describe "getSourceEmail" $ do
    let sourceEmailContext = TestAppContext sourceEmails
    it "succeeds if the key is found" $ do
      email <- runRIO sourceEmailContext $ getSourceEmail "app_code"
      email `shouldBe` unsafeEmailAddress "code" "app.com" 
    it "succeeds if the key is found for the 'any' template" $ do
      email <- runRIO sourceEmailContext $ getSourceEmail "app_foo"
      email `shouldBe` unsafeEmailAddress "any" "app.com"
    it "should throw SourceEmailNotFound if the template is not found" $ do
      let action = runRIO sourceEmailContext $ getSourceEmail "foo_bar"
      action `shouldThrow` sourceEmailNotFoundSelector

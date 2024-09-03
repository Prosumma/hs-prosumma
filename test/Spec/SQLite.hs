{-# LANGUAGE DeriveGeneric, FunctionalDependencies #-}

-- | Tests for SQLite and the Repository pattern.
module Spec.SQLite (testSQLite) where

import Database.SQLite.Simple (FromRow, ToRow)
import Prosumma.SQLite
import RIO
import Test.Hspec

data User = User {
  id :: !Int,
  name :: !Text
} deriving (Eq, Show, Generic)

instance FromRow User where
instance ToRow User where

class UserRepository r where
  createUserSchemaR :: MonadIO m => r -> m ()
  addUserR :: MonadIO m => User -> r -> m ()
  getFirstUserR :: MonadIO m => r -> m User

instance UserRepository Connection where
  createUserSchemaR conn = runRIO conn $ execute_ "CREATE TABLE IF NOT EXISTS user(id INT NOT NULL PRIMARY KEY, name TEXT NOT NULL)" 
  addUserR user conn = runRIO conn $ execute "INSERT INTO user(id, name) SELECT ?, ?" user
  getFirstUserR conn = runRIO conn $ query1_ "SELECT id, name FROM user LIMIT 1"

class UserRepository r => HasUserRepository r env | env -> r where
  getUserRepository :: env -> r 

instance HasUserRepository Connection Connection where
  getUserRepository = RIO.id

withUserRepository :: (MonadReader env m, HasUserRepository r env) => (r -> m a) -> m a
withUserRepository action = asks getUserRepository >>= action

createUserSchema :: (MonadReader env m, HasUserRepository r env, MonadIO m) => m ()
createUserSchema = withUserRepository createUserSchemaR

addUser :: (MonadReader env m, HasUserRepository r env, MonadIO m) => User -> m ()
addUser user = withUserRepository $ addUserR user

getFirstUser :: (MonadReader env m, HasUserRepository r env, MonadIO m) => m User
getFirstUser = withUserRepository getFirstUserR

testSQLite :: Spec
testSQLite = describe "SQLite integration" $ do
  it "works" $ do
    conn <- liftIO $ open ":memory:"
    let input = User 3 "Bob"
    output <- runRIO conn $ createUserSchema >> addUser input >> getFirstUser
    input `shouldBe` output
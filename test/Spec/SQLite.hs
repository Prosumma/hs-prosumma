{-# LANGUAGE DeriveGeneric #-}

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

testSQLite :: Spec
testSQLite = describe "SQLite integration" $ do
  it "works" $ do
    conn <- liftIO $ open ":memory:"
    let input = User 3 "Bob"
    output <- runRIO conn $ do
      execute_ "CREATE TABLE IF NOT EXISTS user(id INT NOT NULL PRIMARY KEY, name TEXT NOT NULL)"
      execute "INSERT INTO user(id, name) SELECT ?, ?" input 
      query1_ "SELECT id, name FROM user LIMIT 1"
    input `shouldBe` output
module Prosumma.Auth.Util (
  putStrLn,
  returnValue
) where

import Data.Aeson
import Data.Aeson.Key
import RIO
import RIO.ByteString

putStrLn :: ByteString -> IO ()
putStrLn s = putStr s >> putStr "\n"

returnValue :: (Monad m, ToJSON v) => Text -> v -> m Value
returnValue key value = return $ object [ fromText key .= value ]

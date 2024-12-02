module Prosumma.Util.IO (
  print,
  putStrLn
) where

import qualified Prelude
import RIO

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . Prelude.putStrLn

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.print

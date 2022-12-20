module Prosumma.Util (
  also,
  fmapMaybeM
) where

import RIO

also :: Monad m => (a -> m b) -> a -> m a
also f a = f a >> return a

fmapMaybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
fmapMaybeM _ Nothing = return Nothing
fmapMaybeM f (Just x) = f x <&> Just
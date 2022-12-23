module Prosumma.Util (
  also,
  fmapMaybeM,
  (<->),
  (<#>)
) where

import RIO

also :: Monad m => (a -> m b) -> a -> m a
also f a = f a >> return a

fmapMaybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
fmapMaybeM _ Nothing = return Nothing
fmapMaybeM f (Just x) = f x <&> Just

-- | Shortcut to create a pair.
(<->) :: a -> b -> (a, b)
a <-> b = (a, b)

infixl 8 <->

-- | Shortcut to create a pair in a list
-- 
-- Useful for creating maps:
--
-- > Map.fromList $ "foo" <#> "bar" <> "bing" <#> "bang"
(<#>) :: a -> b -> [(a, b)]
a <#> b = [a <-> b] 

infixl 7 <#>
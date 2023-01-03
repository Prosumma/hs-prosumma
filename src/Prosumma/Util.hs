module Prosumma.Util (
  also,
  firstJust,
  firstJusts,
  fmapMaybeM,
  (<->),
  (<#>),
  (<=>)
) where

import RIO
import RIO.Map (singleton)

firstJust a b = firstJusts [a, b]

firstJusts :: Foldable f => f (Maybe a) -> Maybe a
firstJusts = msum

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

-- | Shortcut to create a Map from a pair
--
-- Useful for creating maps:
-- > "foo" <=> "bar" <> "bing" <=> "bang"
(<=>) :: k -> v -> Map k v
k <=> v = singleton k v

infixl 7 <=>

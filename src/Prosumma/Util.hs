module Prosumma.Util (
  also,
  coalesce,
  fmapMaybeM,
  maybeFromRight,
  makeProsummaLenses,
  whenNothing,
  whenNothingM,
  Coalesce(..),
  (??~),
  (<->),
  (<#>),
  (<=>),
  (<<$>>),
  (<<&>>)
) where

import Control.Lens hiding ((??), (.~))
import Data.Foldable
import Language.Haskell.TH
import RIO
import RIO.Map (singleton)

makeProsummaLenses :: Name -> DecsQ
makeProsummaLenses = makeLensesWith abbreviatedFields

class Coalesce a where
  (??) :: a -> a -> a 

infixl 1 ??

coalesce :: (Foldable f, Coalesce a) => a -> f a -> a
coalesce = foldr (??)

instance Coalesce (Maybe a) where
  j@(Just _j) ?? _other = j
  Nothing ?? other = other

instance Coalesce (Either e a) where
  r@(Right _r) ?? _other = r
  (Left _e) ?? r@(Right _r) = r
  e1@(Left _e1) ?? (Left _e2) = e1

infixl 1 ??~

(??~) :: (Monad m, Coalesce (m a)) => m a -> a -> m a 
a ??~ b = a ?? return b

-- | Do something as a side effect.
--
-- > foo :: IO Int 
-- > foo = also print 17
--
-- The above is the same as:
--
-- > foo :: IO Int
-- > foo = let x = 17 in print x >> return x
also :: Monad m => (a -> m b) -> a -> m a
also f a = f a >> return a

-- | Monadic `fmap` for `Maybe`.
fmapMaybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
fmapMaybeM _ Nothing = return Nothing
fmapMaybeM f (Just x) = f x <&> Just

maybeFromRight :: Either b a -> Maybe a
maybeFromRight (Right a) = Just a
maybeFromRight _left = Nothing

whenNothing :: Monad m => Maybe a -> m a -> m a
whenNothing cond action = maybe action return cond

whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM cond action = cond >>= maybe action return

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

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <<$>> a = fmap (f <$>) a

infixl 4 <<$>>

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
a <<&>> f = fmap (f <$>) a

infixl 1 <<&>>
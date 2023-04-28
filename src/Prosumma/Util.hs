module Prosumma.Util (
  also,
  firstJust,
  firstJusts,
  fmapMaybeM,
  maybeFromRight,
  makeProsummaLenses,
  whenNothing,
  whenNothingM,
  (??),
  (??~),
  (<->),
  (<#>),
  (<=>),
  (<<$>>),
  (<<&>>)
) where

import Control.Lens hiding ((??), (.~))
import Language.Haskell.TH
import RIO
import RIO.Map (singleton)

makeProsummaLenses :: Name -> DecsQ
makeProsummaLenses = makeLensesWith abbreviatedFields

infixl 1 `firstJust`

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust a b = firstJusts [a, b]

infixl 1 ??

-- | Selects the first `Just`
--
-- > let a = Nothing
-- > let b = Just 7
-- > a ?? b
--
-- The above results in `Just 7`.
(??) :: Maybe a -> Maybe a -> Maybe a
a ?? b = firstJust a b

infixl 1 ??~

(??~) :: Maybe a -> a -> Maybe a
a ??~ b = a ?? Just b

-- | Selects the first of a list of Maybes.
-- 
-- > firstJusts [Nothing, Just 7, Just 5]
--
-- The above results in `Just 7`.
--
-- This is just `msum` for `Maybe` with a
-- prettier, more explicit name.
firstJusts :: Foldable f => f (Maybe a) -> Maybe a
firstJusts = msum

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
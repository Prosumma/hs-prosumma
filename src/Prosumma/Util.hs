module Prosumma.Util (
  also,
  coalesce,
  fmapMaybeM,
  fromTextReader,
  hush,
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
import Data.Either.Extra
import Data.Text.Read
import Data.Foldable
import Language.Haskell.TH
import RIO hiding (Reader)
import RIO.Map (singleton)

hush :: Either e a -> Maybe a
hush = eitherToMaybe

makeProsummaLenses :: Name -> DecsQ
makeProsummaLenses = makeLensesWith abbreviatedFields

class Coalesce a where
  (??) :: a -> a -> a 

infixl 1 ??

coalesce :: (Foldable f, Coalesce a) => a -> f a -> a
coalesce = foldr (??)

instance Coalesce (Maybe a) where
  (??) = (<|>)

instance Coalesce (Either e a) where
  r@(Right _r) ?? _other = r
  (Left _e) ?? r@(Right _r) = r
  e1@(Left _e1) ?? (Left _e2) = e1

infixl 1 ??~

(??~) :: (Applicative m, Coalesce (m a)) => m a -> a -> m a 
a ??~ b = a ?? pure b

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

whenNothing :: Monad m => Maybe a -> m a -> m a
whenNothing cond action = maybe action return cond

whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM cond action = cond >>= maybe action return

fromTextReader :: Reader a -> Text -> Maybe a
fromTextReader reader text = hush (reader text) <&> fst

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
(<<$>>) = fmap . fmap

infixl 4 <<$>>

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) = flip (fmap . fmap)

infixl 1 <<&>>
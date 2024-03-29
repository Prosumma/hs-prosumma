{-# LANGUAGE TupleSections #-}

module Prosumma.Util (
  addL,
  addSuffix,
  also,
  coalesce,
  displayText,
  extractKeys,
  fromTextReader,
  hush,
  makeProsummaLenses,
  makeLensesWith,
  om,
  slipr,
  uformat,
  whenNothing,
  whenNothingM,
  withResource,
  Coalesce(..),
  (??~),
  (?>>=),
  (<->),
  (<#>),
  (<<&>>),
  (<<$>>),
  (<=>),
  (><),
  (>>=>)
) where

import Control.Lens hiding ((??), (.~), (.=))
import Data.Char
import Data.Either.Extra
import Data.Pool (Pool)
import Data.Text.Read
import Data.Foldable
import Formatting
import Language.Haskell.TH
import RIO hiding (Reader)
import RIO.Map (singleton)

import qualified Data.Pool as Pool
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import qualified RIO.HashMap as HashMap

infixr 2 ><

(><) :: Bool -> Bool -> Bool
a >< b = (a || b) && not (a && b)

-- Borrowed from composition-extra, but I don't need the whole library.
slipr :: (a -> b -> c -> d) -> b -> c -> a -> d
slipr f b c a = f a b c

hush :: Either e a -> Maybe a
hush = eitherToMaybe

addL :: LensRules
addL = addSuffix "L" 

addSuffix :: String -> LensRules
addSuffix suffix = defaultFieldRules & lensField .~ suffixFieldNamer suffix
  where
    suffixFieldNamer :: String -> FieldNamer
    suffixFieldNamer suffix _ _ field = maybeToList $ do
      let fieldPart = nameBase field
      -- e.g., LensFooL if "L" is the suffix.
      -- This helps disambiguate if DuplicateRecordFields is
      -- also used in the same module.
      let cls = "Lens" ++ capitalize fieldPart ++ suffix 
      return (MethodName (mkName cls) (mkName (fieldPart ++ suffix)))
    capitalize [] = []
    capitalize (c:cs) = toUpper c : cs

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

whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing cond action = maybe action pure cond

whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM cond action = cond >>= maybe action return

fromTextReader :: Reader a -> Text -> Maybe a
fromTextReader reader text = hush (reader text) <&> fst

infixl 1 ?>>=

(?>>=) :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
ma ?>>= f = do
  ma' <- ma
  case ma' of
    Just a -> f a
    Nothing -> return Nothing

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

-- | The @om@ combinator from `Control.Monad.Extra`.
-- This is chiefly useful in situations like this:
--
-- > foo :: a -> RIO Bar b
-- > foo a = do
-- >   bar <- ask
-- >   baz bar a
--
-- Instead, we can say
--
-- > foo :: a -> RIO Bar b
-- > foo = om bar ask 
--
-- However, in most cases it's easier to use @>>=>@,
-- the flipped, infix version of @om@.
om :: Monad m => (a -> b -> m c) -> m a -> b -> m c
om f a = (a >>=) . flip f

-- | Flipped version of 'om' as an operator.
--
-- This is chiefly useful in situations like this:
--
-- > foo :: a -> RIO Bar b
-- > foo a = do
-- >   bar <- ask
-- >   baz bar a
--
-- Instead, we can say
--
-- > foo :: a -> RIO Bar b
-- > foo = ask >>=> baz
(>>=>) :: Monad m => m a -> (a -> b -> m c) -> b -> m c
(>>=>) = flip om

infixl 1 >>=>

displayText :: Text -> Utf8Builder
displayText = display

uformat :: Format Utf8Builder a -> a
uformat m = runFormat m (display . T.toStrict . T.toLazyText)

withResource :: MonadUnliftIO m => Pool a -> (a -> m b) -> m b
withResource pool action = withRunInIO $ \runInIO -> Pool.withResource pool (runInIO . action)

extractKeys :: Hashable k => Set k -> HashMap k v -> HashMap k v
extractKeys keys hm = HashMap.fromList . mapMaybe extract $ toList keys
  where
    extract k = (k,) <$> HashMap.lookup k hm
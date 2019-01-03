{-# LANGUAGE AllowAmbiguousTypes, GADTs, ImplicitParams, ScopedTypeVariables, TypeApplications, RecordWildCards, RankNTypes, ConstraintKinds #-}
module Pure.Cache
  ( Caching, caching
  , load, store, delete
  ) where

import Pure.Data.Default
import Pure.Data.View
import Pure.Data.View.Patterns

import Data.Typeable
import Data.Proxy

import Pure.Cache.DynamicMap

data Cache where
  Cache ::
    { load_    :: forall v k. (Ord k, Typeable k, Typeable v) => k -> Maybe v
    , store_   :: forall v k. (Ord k, Typeable k, Typeable v) => k -> v -> IO ()
    , delete_  :: forall v k. (Ord k, Typeable k, Typeable v) => Proxy v -> k -> IO ()
    } -> Cache

type Caching = (?__dynamicCache :: Cache)

-- | Lookup a value for the given key in the view-local cache.
load :: forall v k. (Ord k, Typeable k, Typeable v, Caching) => k -> Maybe v
load k = let Cache {..} = ?__dynamicCache in load_ @v k

-- | Store, at the given key, the given value in the view-local cache.
store :: (Ord k, Typeable k, Typeable v, Caching) => k -> v -> IO ()
store k v = let Cache {..} = ?__dynamicCache in store_ k v

-- | Delete, at the given key, from the view-local cache.
delete :: forall v k. (Ord k, Typeable k, Typeable v, Caching) => k -> IO ()
delete k = let Cache {..} = ?__dynamicCache in delete_ (Proxy @v) k

data Cacher = Cacher (Caching => View)

-- | Witnesses a view-local cache of arbitrary typeable keys and values. This
-- is a useful decorator for composing more complex decorators, like fetchers
-- of shared data and suspense-ful subviews. It is advised to create a unique
-- newtype for one of either stored keys or values, to avoid overwrite.
caching :: (Caching => View) -> View
caching v = flip LibraryComponentIO (Cacher v) $ \self ->
  let
    storer :: forall v k. (Ord k, Typeable k, Typeable v) => k -> v -> IO ()
    storer k v = modify_ self $ \_ -> storeDyn k v

    deleter :: forall v k. (Ord k, Typeable k, Typeable v) => Proxy v -> k -> IO ()
    deleter _ k = modify_ self $ \_ -> deleteDyn @v k
  in
    def
      { construct = return (DynamicMap mempty)
      , render = \(Cacher v) dm ->
        let loader :: forall v k. (Ord k, Typeable k, Typeable v) => k -> Maybe v
            loader k = loadDyn k dm
        in let ?__dynamicCache = Cache loader storer deleter
           in v
      }

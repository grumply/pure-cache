{-# LANGUAGE AllowAmbiguousTypes, GADTs, ImplicitParams, ScopedTypeVariables, TypeApplications, RecordWildCards, RankNTypes, ConstraintKinds, BangPatterns #-}
module Pure.Cache
  ( Caching, caching
  , Cache
  , load, loadIO, store, seed, delete, flush, flushAll, clear
  , deleteProxy, flushAllProxy
  ) where

import Pure.Data.Default
import Pure.Data.View
import Pure.Data.View.Patterns

import Data.Typeable
import Data.Proxy

import Pure.Cache.DynamicMap

data Cache where
  Cache ::
    { load_     :: forall v k. (Ord k, Typeable k, Typeable v) => k -> Maybe v
    , loadIO_   :: forall v k. (Ord k, Typeable k, Typeable v) => k -> IO (Maybe v)
    , store_    :: forall v k. (Ord k, Typeable k, Typeable v) => k -> v -> IO ()
    , seed_     :: forall v k. (Ord k, Typeable k, Typeable v) => k -> v -> IO ()
    , delete_   :: forall v k. (Ord k, Typeable k, Typeable v) => Proxy v -> k -> IO ()
    , flush_    :: forall   k. (Ord k, Typeable k)             => k -> IO ()
    , flushAll_ :: forall   k.         Typeable k              => Proxy k -> IO ()
    , clear_    ::                                                IO ()
    } -> Cache

type Caching = (?__dynamicCache :: Cache)

-- | Lookup a value of type `v` at the given key of type `k`.
load :: forall v k. (Ord k, Typeable k, Typeable v, Caching) => k -> Maybe v
load k = let Cache {..} = ?__dynamicCache
         in let !r = load_ @v k
            in r

-- | Lookup a value of type `v` at the given key of type `k`.
loadIO :: forall v k. (Ord k, Typeable k, Typeable v, Caching) => k -> IO (Maybe v)
loadIO k = let Cache {..} = ?__dynamicCache
           in loadIO_ k

-- | Store a given value of type `v` at the given key of type `k`.
store :: (Ord k, Typeable k, Typeable v, Caching) => k -> v -> IO ()
store k v = let Cache {..} = ?__dynamicCache in store_ k v

-- | Store a given value of type `v` at the given key of type `k` without forcing re-render
seed :: (Ord k, Typeable k, Typeable v, Caching) => k -> v -> IO ()
seed k v = let Cache {..} = ?__dynamicCache in seed_ k v

-- | Delete a value of type `v` at the given key of type `k`.
--
-- Use will require a visible type application for `v`:
--
-- > delete @Int someKey
-- > --     ^ the type of value that will be deleted
--
delete :: forall v k. (Ord k, Typeable k, Typeable v, Caching) => k -> IO ()
delete k = let Cache {..} = ?__dynamicCache in delete_ (Proxy @v) k

-- | Delete a value of type `v` at the given key of type `k`.
deleteProxy :: forall v k. (Ord k, Typeable k, Typeable v, Caching) => Proxy v -> k -> IO ()
deleteProxy _ k = let Cache {..} = ?__dynamicCache in delete_ (Proxy @v) k

-- | Delete all values of all types stored at the given key `k`.
flush :: forall k. (Ord k, Typeable k, Caching) => k -> IO ()
flush k = let Cache {..} = ?__dynamicCache in flush_ k

-- | Delete all values of all types stored at all keys of type `k`.
--
-- Use will require a visible type application for `k`.
--
-- > flushAll @Int
-- >          ^ delete everything with an Int key
--
flushAll :: forall k. (Typeable k, Caching) => IO ()
flushAll = let Cache {..} = ?__dynamicCache in flushAll_ (Proxy @k)

-- | Delete all values of all types stored at all keys of type `k`.
flushAllProxy :: forall k. (Typeable k, Caching) => Proxy k -> IO ()
flushAllProxy p = let Cache {..} = ?__dynamicCache in flushAll_ p

-- | Delete all values of all types stored at all keys.
clear :: Caching => IO ()
clear = let Cache {..} = ?__dynamicCache in clear_

data Cacher = Cacher (Caching => View)

-- | Witnesses a view-local cache of arbitrary dynamic keys and dynamic values.
--
-- The cache is equivalent to this:
--
-- > (Typeable key, Ord key, Typeable value)
-- >     => TypeRep key -> (key -> (TypeRep value -> value))
--
-- Supplying the cache in an implicit context is a convenience to avoid the need
-- to pass it around as a function parameter. But, this means nesting caches can
-- cause confusion, so take care in where you place your cache in the hierarchy.
caching :: (Caching => View) -> View
caching v = flip Component (Cacher v) $ \self ->
  let
    store :: forall v k. (Ord k, Typeable k, Typeable v) => k -> v -> IO ()
    store k v = modify_ self $ \_ (dm,_) -> (storeDyn k v dm,True)

    loadIO :: forall v k. (Ord k, Typeable k, Typeable v) => k -> IO (Maybe v)
    loadIO k = do
      (dm,_) <- get self
      return $ loadDyn k dm

    seed :: forall v k. (Ord k, Typeable k, Typeable v) => k -> v -> IO ()
    seed k v = modify_ self $ \_ (dm,_) -> (storeDyn k v dm,False)

    delete :: forall v k. (Ord k, Typeable k, Typeable v) => Proxy v -> k -> IO ()
    delete _ k = modify_ self $ \_ (dm,_) -> (deleteDyn @v k dm,True)

    flush :: forall k. (Ord k, Typeable k) => k -> IO ()
    flush k = modify_ self $ \_ (dm,_) -> (flushDyn k dm,True)

    flushAll :: forall k. Typeable k => Proxy k -> IO ()
    flushAll _ = modify_ self $ \_ (dm,_) -> (flushAllDyn @k dm,True)

    clear :: IO ()
    clear = modify_ self $ \_ (dm,_) -> (clearDyn dm,True)

  in
    def
      { construct = return (DynamicMap mempty,True)
      , force = \_ (_,f) -> return f
      , render = \(Cacher v) (dm,_) ->
        -- only load is scoped inside the renderer;
        -- uses of any of the Cache record fields
        -- other than load should not force re-render.
        let load :: forall v k. (Ord k, Typeable k, Typeable v) => k -> Maybe v
            load k = let !r = loadDyn k dm in r
        in let ?__dynamicCache = Cache load loadIO store seed delete flush flushAll clear
           in v
      }

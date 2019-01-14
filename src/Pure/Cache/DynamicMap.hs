{-# LANGUAGE AllowAmbiguousTypes, GADTs, ScopedTypeVariables, TypeApplications #-}
module Pure.Cache.DynamicMap where

import Data.Dynamic
import qualified Data.Typeable as Typeable
import Type.Reflection as Reflection
import Data.Proxy

import Data.Map.Strict as Map

data OrdDynamic where
  OrdDynamic :: (Eq a, Ord a) => Reflection.TypeRep a -> a -> OrdDynamic

instance Eq OrdDynamic where
  (==) (OrdDynamic t1 a) (OrdDynamic t2 b)
    | Just HRefl <- t1 `eqTypeRep` t2 = a == b
    | otherwise = error "Pure.Cache: Eq(==)@OrdDynamic: Incomparable"

instance Ord OrdDynamic where
  compare (OrdDynamic t1 a) (OrdDynamic t2 b)
    | Just HRefl <- t1 `eqTypeRep` t2 = compare a b
    | otherwise = error "Pure.Cache: Ord(compare)@OrdDynamic: Incomparable"

toOrdDyn :: (Typeable a, Ord a) => a -> OrdDynamic
toOrdDyn a = OrdDynamic (typeOf a) a

fromOrdDyn :: forall a. (Typeable a, Ord a) => OrdDynamic -> Maybe a
fromOrdDyn (OrdDynamic t a)
  | Just HRefl <- t `eqTypeRep` rep = Just a
  | otherwise                       = Nothing
  where
    rep = typeRep :: TypeRep a

type TypedMap v = Map Typeable.TypeRep v
type DynamicOrdMap = Map OrdDynamic (TypedMap Dynamic)
data DynamicMap = DynamicMap (TypedMap DynamicOrdMap)
-- Basically this:
--   (Typeable key, Ord key, Typeable value)
--      => TypeRep key -> (key -> (TypeRep value -> value))

empty :: DynamicMap
empty = DynamicMap mempty

-- | Lookup a value of type `v` at the given key of type `k`.
-- Use may require a type signature or visible type application for `v`.
loadDyn :: forall v k. (Ord k, Typeable k, Typeable v) => k -> DynamicMap -> Maybe v
loadDyn k (DynamicMap dm) = do
  m1 <- Map.lookup repk dm
  m2 <- Map.lookup (toOrdDyn k) m1
  dv <- Map.lookup repv m2
  fromDynamic dv
  where
    repk = Typeable.typeOf (undefined :: k)
    repv = Typeable.typeOf (undefined :: v)

-- | Store a given value of type `v` at the given key of type `k`.
storeDyn :: forall k v. (Ord k, Typeable k, Typeable v) => k -> v -> DynamicMap -> DynamicMap
storeDyn k v (DynamicMap dm) = DynamicMap (Map.alter alter repk dm)
  where
    repk = Typeable.typeOf (undefined :: k)
    repv = Typeable.typeOf (undefined :: v)

    odk = toOrdDyn k
    dv = toDyn v

    alter :: Maybe (Map OrdDynamic (Map Typeable.TypeRep Dynamic)) -> Maybe (Map OrdDynamic (Map Typeable.TypeRep Dynamic))
    alter Nothing  = Just (Map.singleton odk (Map.singleton repv dv))
    alter (Just m) = Just (Map.alter alter' odk m)
      where
        alter' Nothing  = Just (Map.singleton repv dv)
        alter' (Just m) = Just (Map.insert repv dv m)

-- | Delete a value of type `v` at the given key of type `k`.
--
-- Use will require a visible type application for `v`:
--
-- > deleteDyn @Int someKey someDynamicMap
-- > --        ^ the type of value that will be deleted
--
deleteDyn :: forall v k. (Ord k, Typeable k, Typeable v) => k -> DynamicMap -> DynamicMap
deleteDyn k (DynamicMap dm) = DynamicMap (Map.update update repk dm)
  where
    repk = Typeable.typeOf (undefined :: k)
    repv = Typeable.typeOf (undefined :: v)

    odk = toOrdDyn k

    -- self-cleaning deletion; avoids holding onto empty maps
    update :: Map OrdDynamic (Map Typeable.TypeRep Dynamic) -> Maybe (Map OrdDynamic (Map Typeable.TypeRep Dynamic))
    update m =
      let m' = Map.update update' odk m
      in case Map.size m' of
          0 -> Nothing
          _ -> Just m'
      where
        update' m =
          let m' = Map.delete repv m
          in case Map.size m' of
              0 -> Nothing
              _ -> Just m'

-- | Delete a value of type `v` at the given key of type `k`.
deleteDynProxy :: forall v k. (Ord k, Typeable k, Typeable v) => Proxy v -> k -> DynamicMap -> DynamicMap
deleteDynProxy _ = deleteDyn @v

-- | Delete all values of all types stored at the given key `k`.
flushDyn :: forall k. (Ord k, Typeable k) => k -> DynamicMap -> DynamicMap
flushDyn k (DynamicMap dm) = DynamicMap (Map.update update repk dm)
  where
    repk = Typeable.typeOf (undefined :: k)

    odk = toOrdDyn k

    update :: Map OrdDynamic (Map Typeable.TypeRep Dynamic) -> Maybe (Map OrdDynamic (Map Typeable.TypeRep Dynamic))
    update m =
      let m' = Map.delete odk m
      in case Map.size m' of
           0 -> Nothing
           _ -> Just m'

-- | Delete all values of all types stored at all keys of type `k`.
--
-- Use will require a visible type application for `k`.
--
-- > flushAllDyn @Int someDynamicMap
-- >             ^ delete everything with an Int key
--
flushAllDyn :: forall k. Typeable k => DynamicMap -> DynamicMap
flushAllDyn (DynamicMap dm) = DynamicMap (Map.delete repk dm)
  where
    repk = Typeable.typeOf (undefined :: k)

-- | Delete all values of all types stored at all keys of type `k`.
flushAllDynProxy :: forall k. (Ord k, Typeable k) => Proxy k -> DynamicMap -> DynamicMap
flushAllDynProxy _ = flushAllDyn @k

-- | Delete all values of all types stored at all keys.
--
-- Equivalent to `const empty`.
clearDyn :: DynamicMap -> DynamicMap
clearDyn _ = Pure.Cache.DynamicMap.empty

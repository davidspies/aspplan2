module IData.IMap
  ( IMap
  , (!)
  , (!?)
  , differenceWith
  , elems
  , empty
  , filter
  , fromListWith
  , intersectionWith
  , keysSet
  , logMerge
  , map
  , mapConst
  , mapMaybe
  , mapWithKey
  , null
  , restrictKeys
  , singleton
  , size
  , subsetKeys
  , toList
  , unionWith
  , unionsWith
  , withoutKeys
  )
where

import           Data.Bifunctor                 ( first )
import qualified Data.IntMap.Strict            as IntMap
import           Data.Maybe                     ( fromMaybe )
import           GHC.Stack                      ( HasCallStack )
import           Prelude                 hiding ( map
                                                , null
                                                , filter
                                                )
import qualified Prelude                       as P

import           IData.ISet.Internal            ( ISet(..) )
import           IData.ISet                     ( logMerge )
import           IData.IMap.Internal
import           IData.IsInt.Impl

empty :: IMap k v
empty = IMap IntMap.empty

filter :: (v -> Bool) -> IMap k v -> IMap k v
filter f (IMap x) = IMap $ IntMap.filter f x

fromListWith :: IsInt k => (v -> v -> v) -> [(k, v)] -> IMap k v
fromListWith f xs = IMap $ IntMap.fromListWith f (P.map (first toInt) xs)

singleton :: IsInt k => k -> v -> IMap k v
singleton k v = IMap $ IntMap.singleton (toInt k) v

map :: (v -> w) -> IMap k v -> IMap k w
map f (IMap m) = IMap $ IntMap.map f m

mapWithKey :: IsInt k => (k -> v -> w) -> IMap k v -> IMap k w
mapWithKey f (IMap m) = IMap $ IntMap.mapWithKey (f . fromInt) m

elems :: IMap k v -> [v]
elems (IMap m) = IntMap.elems m

unionsWith :: forall k v . (v -> v -> v) -> [IMap k v] -> IMap k v
unionsWith f = logMerge empty (unionWith f)

size :: IMap k v -> Int
size (IMap m) = IntMap.size m

null :: IMap k v -> Bool
null (IMap m) = IntMap.null m

mapMaybe :: (v -> Maybe w) -> IMap k v -> IMap k w
mapMaybe f (IMap m) = IMap $ IntMap.mapMaybe f m

intersectionWith :: (a -> b -> c) -> IMap k a -> IMap k b -> IMap k c
intersectionWith f (IMap x) (IMap y) = IMap $ IntMap.intersectionWith f x y

unionWith :: (v -> v -> v) -> IMap k v -> IMap k v -> IMap k v
unionWith f (IMap x) (IMap y) = IMap $ IntMap.unionWith f x y

(!) :: (HasCallStack, IsInt k) => IMap k v -> k -> v
(!) (IMap m) k = fromMaybe
  (error "!: given key is not an element in the map")
  (m IntMap.!? toInt k)

(!?) :: IsInt k => IMap k v -> k -> Maybe v
(!?) (IMap m) k = m IntMap.!? toInt k

differenceWith :: (v -> w -> Maybe v) -> IMap k v -> IMap k w -> IMap k v
differenceWith f (IMap x) (IMap y) = IMap $ IntMap.differenceWith f x y

restrictKeys :: IMap k v -> ISet k -> IMap k v
restrictKeys (IMap x) (ISet y) = IMap (IntMap.restrictKeys x y)

keysSet :: IMap k v -> ISet k
keysSet (IMap x) = ISet $ IntMap.keysSet x

subsetKeys :: (v -> Bool) -> IMap k v -> ISet k
subsetKeys f = keysSet . filter f

withoutKeys :: IMap k v -> ISet k -> IMap k v
withoutKeys (IMap x) (ISet y) = IMap $ x `IntMap.withoutKeys` y

mapConst :: v -> ISet k -> IMap k v
mapConst v (ISet x) = IMap $ IntMap.fromSet (const v) x

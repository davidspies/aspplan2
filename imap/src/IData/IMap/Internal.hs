module IData.IMap.Internal
  ( IMap(..)
  , toList
  )
where

import           Data.Bifunctor                 ( first )
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.Map                      as Map

import           IData.IsInt.Impl

newtype IMap k v = IMap (IntMap v)
  deriving (Eq)

newtype IntOrder a = IntOrder a
  deriving newtype Show

instance IsInt a => Eq (IntOrder a) where
  (==) (IntOrder x) (IntOrder y) = (==) (toInt x) (toInt y)
instance IsInt a => Ord (IntOrder a) where
  compare (IntOrder x) (IntOrder y) = compare (toInt x) (toInt y)

instance (IsInt k, Show k, Show v) => Show (IMap k v) where
  showsPrec d m = showsPrec d $ Map.fromList $ map (first IntOrder) $ toList m

toList :: IsInt k => IMap k v -> [(k, v)]
toList (IMap m) = map (first fromInt) $ IntMap.toList m

module IData.BiMap
  ( BiMap
  , (!)
  , (\\)
  , count
  , empty
  , fromList
  , null
  , pickRows
  , intersection
  , toList
  , transpose
  , union
  , unions
  )
where

import           Prelude                 hiding ( null )

import           Data.Bifunctor                 ( second )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.IntMap                   as IntMap
import qualified Data.IntSet                   as IntSet

import           IData.IMap                     ( IMap
                                                , empty
                                                )
import           IData.IMap.Internal            ( IMap(..) )
import qualified IData.IMap                    as IMap
import           IData.ISet.Internal            ( ISet(..) )
import qualified IData.ISet                    as ISet
import           IData.IsInt                    ( IsInt )

type BiMap a b = IMap a (ISet b)

pickRows :: ISet a -> BiMap a b -> BiMap a b
pickRows x y =
  IMap.unionWith const (IMap.restrictKeys y x) (IMap.mapConst ISet.empty x)

intersection, union, (\\) :: BiMap a b -> BiMap a b -> BiMap a b
intersection = IMap.intersectionWith ISet.intersection
union = IMap.unionWith ISet.union
(\\) = IMap.differenceWith (\x y -> Just $ x ISet.\\ y)

unions :: [BiMap a b] -> BiMap a b
unions = IMap.unionsWith ISet.union

fromList :: (IsInt a, IsInt b) => [(a, b)] -> BiMap a b
fromList xys = IMap.fromListWith ISet.union $ map (second ISet.singleton) xys

toList :: (IsInt a, IsInt b) => BiMap a b -> [(a, b)]
toList m = [ (x, y) | (x, v) <- IMap.toList m, y <- ISet.toList v ]

(!) :: IsInt a => BiMap a b -> a -> ISet b
(!) m x = fromMaybe ISet.empty (m IMap.!? x)

transpose :: BiMap a b -> BiMap b a
transpose (IMap m) = unions
  [ IMap.mapConst (ISet $ IntSet.singleton k) vs | (k, vs) <- IntMap.toList m ]

count :: BiMap a b -> Int
count = sum . map ISet.size . IMap.elems

null :: BiMap a b -> Bool
null = all ISet.null . IMap.elems

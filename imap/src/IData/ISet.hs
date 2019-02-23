module IData.ISet
  ( ISet
  , (\\)
  , delete
  , empty
  , filter
  , fromList
  , insert
  , intersection
  , intersections
  , intersects
  , isSubsetOf
  , logMerge
  , member
  , null
  , singleton
  , size
  , toList
  , union
  , unions
  )
where

import           Data.Coerce                    ( coerce )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NonEmpty
import           Prelude                 hiding ( filter
                                                , null
                                                )

import           IData.ISet.Internal            ( ISet(..) )
import           IData.IsInt.Impl               ( IsInt(..) )

intersection :: ISet k -> ISet k -> ISet k
intersection (ISet x) (ISet y) = ISet (IntSet.intersection x y)

union :: ISet k -> ISet k -> ISet k
union (ISet x) (ISet y) = ISet (IntSet.union x y)

singleton :: IsInt k => k -> ISet k
singleton = ISet . IntSet.singleton . toInt

toList :: IsInt k => ISet k -> [k]
toList (ISet x) = map fromInt $ IntSet.toList x

unions :: [ISet k] -> ISet k
unions xs = ISet $ IntSet.unions (coerce xs :: [IntSet])

isSubsetOf :: ISet k -> ISet k -> Bool
isSubsetOf (ISet x) (ISet y) = IntSet.isSubsetOf x y

intersections :: NonEmpty (ISet k) -> ISet k
intersections = logMerge (error "unreachable") intersection . NonEmpty.toList

(\\) :: ISet k -> ISet k -> ISet k
(\\) (ISet x) (ISet y) = ISet $ x IntSet.\\ y

fromList :: IsInt k => [k] -> ISet k
fromList xs = ISet $ IntSet.fromList $ map toInt xs

filter :: IsInt k => (k -> Bool) -> ISet k -> ISet k
filter f (ISet xs) = ISet $ IntSet.filter (f . fromInt) xs

insert :: IsInt k => k -> ISet k -> ISet k
insert k (ISet s) = ISet $ IntSet.insert (toInt k) s

member :: IsInt k => k -> ISet k -> Bool
member k (ISet s) = IntSet.member (toInt k) s

intersects :: ISet k -> ISet k -> Bool
intersects x y = not $ null (intersection x y)

null :: ISet k -> Bool
null (ISet x) = IntSet.null x

empty :: ISet x
empty = ISet IntSet.empty

size :: ISet x -> Int
size (ISet x) = IntSet.size x

logMerge :: forall a . a -> (a -> a -> a) -> [a] -> a
logMerge emptyV appendV ms0 = fst $ go (length ms0) ms0
 where
  go :: Int -> [a] -> (a, [a])
  go 0 xs       = (emptyV, xs)
  go _ []       = error "unreachable"
  go 1 (x : xs) = (x, xs)
  go n xs =
    let hlen   = n `quot` 2
        (a, i) = go hlen xs
        (b, r) = go (n - hlen) i
    in  (a `appendV` b, r)

delete :: IsInt k => k -> ISet k -> ISet k
delete k (ISet s) = ISet $ IntSet.delete (toInt k) s

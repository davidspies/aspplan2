module Mutex.Fluent
  ( FluentId
  , generateMap
  )
where

import           IData.IMap                     ( IMap )
import qualified IData.IMap                    as IMap
import           IData.IsInt

newtype FluentId = FluentId Int
  deriving Eq
  deriving newtype Show

_fluentid :: Int -> FluentId
_fluentid = FluentId

instance IsInt FluentId

generateMap :: [a] -> IMap FluentId a
generateMap = IMap.fromListWith const . zip (map FluentId [0 ..])

module Mutex.Act
  ( Action(..)
  , ActionId
  , generateMap
  )
where

import           IData.IMap                     ( IMap )
import qualified IData.IMap                    as IMap
import           IData.ISet                     ( ISet )
import           IData.IsInt
import           Mutex.Fluent                   ( FluentId )

newtype ActionId = ActionId Int
  deriving Eq
  deriving newtype Show
_actionid :: Int -> ActionId
_actionid = ActionId

instance IsInt ActionId

data Action name = Action
  { name :: name
  , pre :: ISet FluentId
  , add :: ISet FluentId
  , del :: ISet FluentId
  }
  deriving (Show)

generateMap :: [Action name] -> IMap ActionId (Action name)
generateMap = IMap.fromListWith const . zip (map ActionId [0 ..])

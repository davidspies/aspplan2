module IData.ISet.Internal
  ( ISet(..)
  )
where

import           Data.IntSet                    ( IntSet )

newtype ISet k = ISet IntSet
  deriving Eq
  deriving newtype Show

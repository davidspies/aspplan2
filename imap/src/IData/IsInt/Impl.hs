module IData.IsInt.Impl
  ( IsInt(..)
  )
where

import           Data.Coerce

class IsInt a where
  fromInt :: Int -> a
  default fromInt :: Coercible Int a => Int -> a
  fromInt = coerce
  toInt :: a -> Int
  default toInt :: Coercible Int a => a -> Int
  toInt = coerce

instance IsInt Int

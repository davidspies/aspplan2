module IData.Graph.Internal
  ( EdgeMap
  , Graph(..)
  , LabeledGraph(..)
  , Vertex
  , mkGraph
  )
where

import qualified Data.Map                      as Map
import           Data.Tuple                     ( swap )

import           IData.BiMap                    ( BiMap )
import qualified IData.BiMap                   as BiMap
import           IData.IMap                     ( IMap )
import qualified IData.IMap                    as IMap
import           IData.ISet                     ( ISet )
import           IData.IsInt                    ( IsInt )

newtype Vertex = Vertex Int
  deriving newtype Show
instance IsInt Vertex

type EdgeMap = BiMap Vertex Vertex

data Graph = Graph
  { vertices :: ISet Vertex
  , edges :: EdgeMap
  }
  deriving (Show)

data LabeledGraph vert = LabeledGraph Graph (IMap Vertex vert)

mkGraph :: Ord vert => [vert] -> [(vert, vert)] -> LabeledGraph vert
mkGraph vs es = LabeledGraph Graph {..} labels
 where
  labels = IMap.fromListWith (error "unreachable") $ zip (map Vertex [0 ..]) vs
  vertices = IMap.keysSet labels
  backMap = Map.fromList $ map swap $ IMap.toList labels
  edges = BiMap.fromList [ (backMap Map.! l, backMap Map.! r) | (l, r) <- es ]

module IData.Graph
  ( complement
  , connectedComponents
  , inducedSubgraph
  , topologicSort
  , module X
  )
where

import           Control.Monad.ST
import           Data.Maybe                     ( catMaybes )
import           Data.STRef

import qualified IData.BiMap                   as BiMap
import           IData.Graph.Internal          as X
import qualified IData.IMap                    as IMap
import           IData.ISet                     ( ISet )
import qualified IData.ISet                    as ISet

connectedComponents :: Graph -> [ISet Vertex]
connectedComponents Graph {..} = runST go
 where
  go :: forall s . ST s [ISet Vertex]
  go = do
    used :: STRef s (ISet Vertex) <- newSTRef ISet.empty
    let buildPartition :: Vertex -> ST s (ISet Vertex)
        buildPartition x = do
          isUsed <- (x `ISet.member`) <$> readSTRef used
          if isUsed
            then pure ISet.empty
            else do
              modifySTRef used (ISet.insert x)
              ISet.unions . (ISet.singleton x :) <$> mapM
                buildPartition
                (ISet.toList $ edges IMap.! x)
    catMaybes <$> mapM
      ( fmap (\part -> if ISet.null part then Nothing else Just part)
      . buildPartition
      )
      (ISet.toList vertices)

inducedSubgraph :: Graph -> ISet Vertex -> Graph
inducedSubgraph Graph { edges } vertices = Graph
  { vertices
  , edges    = IMap.map (`ISet.intersection` vertices)
                        (IMap.restrictKeys edges vertices)
  }

complement :: Graph -> Graph
complement Graph { vertices, edges } =
  Graph {vertices , edges = IMap.mapConst vertices vertices BiMap.\\ edges}

topologicSort :: Graph -> [Vertex]
topologicSort Graph {..} = runST go
 where
  go :: forall s . ST s [Vertex]
  go = do
    visited :: STRef s (ISet Vertex) <- newSTRef ISet.empty
    let visit v = do
          alreadyDone <- (v `ISet.member`) <$> readSTRef visited
          if alreadyDone
            then return []
            else do
              modifySTRef' visited (ISet.insert v)
              (v :) <$> revCatMapM visit (ISet.toList $ edges BiMap.! v)
    revCatMapM visit (ISet.toList vertices)

revCatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
revCatMapM f = fmap concat . revMapM f

revMapM :: Monad m => (a -> m b) -> [a] -> m [b]
revMapM f = fmap reverse . mapM f . reverse

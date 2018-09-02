module Mutex.MCFinder
  ( findCover
  , retrieveMCLabels
  )
where

import           Data.List                      ( maximumBy
                                                , partition
                                                )
import           Data.List.NonEmpty             ( nonEmpty )
import           Debug.Trace                    ( trace )

import           IData.Graph
import qualified IData.BiMap                   as BiMap
import           IData.IMap                     ( IMap )
import qualified IData.IMap                    as IMap
import           IData.ISet                     ( ISet )
import qualified IData.ISet                    as ISet

type MCPartition = ISet Vertex
type MultiClique = [MCPartition]

edgesCoveredBy :: EdgeMap -> MultiClique -> EdgeMap
edgesCoveredBy unused mc = BiMap.intersection unused
  $ BiMap.unions [ IMap.mapConst (vs ISet.\\ p) p | p <- mc ]
  where vs = ISet.unions mc

defaultsFor :: Graph -> EdgeMap -> ISet Vertex -> MCPartition
defaultsFor g uncovered vs = usefulSubset uncovered vs (preDefaultsFor g vs)

preDefaultsFor :: Graph -> ISet Vertex -> MCPartition
preDefaultsFor Graph { edges } vs =
  maybe ISet.empty ISet.intersections
    $ nonEmpty
    $ IMap.elems
    $ IMap.restrictKeys edges vs

usefulSubset :: EdgeMap -> ISet Vertex -> ISet Vertex -> ISet Vertex
usefulSubset uncovered vs predefs = IMap.subsetKeys useful
  $ IMap.restrictKeys uncovered predefs
 where
  useful :: ISet Vertex -> Bool
  useful u = ISet.size (ISet.intersection u vs) >= 2

score :: EdgeMap -> MultiClique -> Int
score uncovered mc = newlyCoveredCount - complexityCost
 where
  newlyCoveredCount = BiMap.count (edgesCoveredBy uncovered mc)
  complexityCost =
    sum $ map (\(ISet.size -> sp) -> if sp == 1 then 1 else 2 * sp + 1) mc

makeMultiClique :: Graph -> ISet Vertex -> MultiClique
makeMultiClique g vs = connectedComponents $ complement $ inducedSubgraph g vs

nextMultiClique :: Graph -> EdgeMap -> MultiClique
nextMultiClique g@Graph { edges } unused = makeMultiClique g
  $ go (ISet.singleton startVert)
 where
  go :: ISet Vertex -> ISet Vertex
  go verts
    | ISet.null candidates || bestScore < curScore
    = verts `ISet.union` defaultsFor g unused verts
    | otherwise
    = go (ISet.insert bestCandidate verts)
   where
    predefaults = preDefaultsFor g verts
    curMC = makeMultiClique g verts
    curScore = score unused (curMC ++ [usefulSubset unused verts predefaults])
    candidates =
      ISet.unions (IMap.elems $ IMap.restrictKeys unused verts) ISet.\\ verts
    (bestCandidate, bestScore) = maximumOn
      snd
      [ (x, score unused (insertedMC ++ [defaults]))
      | (x, ex) <- IMap.toList $ IMap.restrictKeys edges candidates
      , let insertedMC = addToMC x ex curMC
            defaults = usefulSubset unused
                                    (ISet.insert x verts)
                                    (predefaults `ISet.intersection` ex)
      ]
  startVert =
    fst $ maximumOn snd $ IMap.toList $ IMap.map ISet.size $ BiMap.intersection
      edges
      unused

addToMC :: Vertex -> ISet Vertex -> MultiClique -> MultiClique
addToMC v ev mc = ISet.unions (ISet.singleton v : r) : l
  where (l, r) = partition (`ISet.isSubsetOf` ev) mc

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (\x y -> compare (f x) (f y))

findCover :: Graph -> [MultiClique]
findCover g@Graph { edges } = go 0 edges
 where
  go :: Int -> EdgeMap -> [MultiClique]
  go count unused = if all ISet.null $ IMap.elems unused
    then trace "multicliques finished" []
    else
      trace
          (  "Begin multiclique "
          ++ show count
          ++ "; "
          ++ show (BiMap.count unused `quot` 2)
          ++ " edges remaining."
          )
        $ let n = nextMultiClique g unused
          in  n : go (count + 1) (unused BiMap.\\ edgesCoveredBy unused n)

retrieveMCLabels :: IMap Vertex vert -> MultiClique -> [[vert]]
retrieveMCLabels labels = map $ \p -> IMap.elems $ labels `IMap.restrictKeys` p

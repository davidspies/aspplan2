module Mutex.Prob
  ( genMutex
  , makeMCs
  , mcRules
  , mutexRule
  , probify
  )
where

import           Clingo.Symbol                  ( PureSymbol(..) )
import           Data.Bifunctor                 ( second )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Data.Tuple                     ( swap )
import           GHC.Stack                      ( HasCallStack )

import           IData.Graph
import qualified IData.IMap                    as IMap
import           IData.ISet                     ( ISet )
import qualified IData.ISet                    as ISet
import           Mutex.MCFinder
import           Mutex                          ( Prob(..)
                                                , mutex
                                                )
import           Mutex.Act                      ( Action(..) )
import qualified Mutex.Act                     as Act
import           Mutex.Fluent                   ( FluentId )
import qualified Mutex.Fluent                  as Fluent

partitionByName :: [PureSymbol] -> Map Text [[PureSymbol]]
partitionByName fs = Map.fromListWith (++) $ do
  f <- fs
  case f of
    PureFunction name args True -> [(name, [args])]
    _                           -> []

probify :: [PureSymbol] -> Prob PureSymbol PureSymbol
probify fs = Prob {..}
 where
  partitioned  = partitionByName fs
  fluents = Fluent.generateMap . map fromSingleton $ partitioned !^ "fluent"
  fluentByName = Map.fromList . map swap $ IMap.toList fluents
  inits        = ISet.fromList
    $ map ((fluentByName !^) . fromSingleton) (partitioned !^ "init")
  mkRelation :: Text -> Map PureSymbol (ISet FluentId)
  mkRelation cat = Map.fromListWith ISet.union $ map
    (second (ISet.singleton . (fluentByName !^)) . fromDoubleton)
    (partitioned !^ cat)
  pres, adds, dels :: Map PureSymbol (ISet FluentId)
  pres = mkRelation "pre"
  adds = mkRelation "add"
  dels = mkRelation "del"
  actions =
    Act.generateMap $ map (mkFn . fromSingleton) $ partitioned !^ "action"
  mkFn :: PureSymbol -> Action PureSymbol
  mkFn name = Action
    { name
    , pre  = fromMaybe ISet.empty (pres Map.!? name)
    , add  = fromMaybe ISet.empty (adds Map.!? name)
    , del  = fromMaybe ISet.empty (dels Map.!? name)
    }

fromSingleton :: [PureSymbol] -> PureSymbol
fromSingleton = \case
  [f]  -> f
  args -> error $ "fromSingleton has wrong number of args: " ++ show args

fromDoubleton :: [PureSymbol] -> (PureSymbol, PureSymbol)
fromDoubleton = \case
  [f, g] -> (f, g)
  args   -> error $ "fromDoubleton has wrong number of args: " ++ show args

function :: Text -> [PureSymbol] -> PureSymbol
function name args = PureFunction name args True

mcRules :: [[[PureSymbol]]] -> [PureSymbol]
mcRules mcs = do
  (i, mc) <- zip [0 ..] mcs
  case mc of
    [[x], [y]] -> [function "mutex" [x, y]]
    _ ->
      let mcname = PureNumber i
      in  function "isMultiClique" [mcname] : do
            (j, p) <- zip [0 ..] mc
            case p of
              [x] -> [function "isSingletonPartIn" [x, mcname]]
              _ ->
                let pname = function "part" [mcname, PureNumber j]
                in  function "isPartitionIn" [pname, mcname]
                      : [ function "inPart" [x, pname] | x <- p ]

mutexRule :: (PureSymbol, PureSymbol) -> PureSymbol
mutexRule (x, y) = function "mutex" [x, y]

(!^) :: (HasCallStack, Ord k) => Map k v -> k -> v
(!^) m k =
  fromMaybe (error "!^: given key is not an element in the map") (m Map.!? k)

genMutex :: [PureSymbol] -> ([PureSymbol], [(PureSymbol, PureSymbol)])
genMutex inp =
  ( IMap.elems (fluents `IMap.restrictKeys` fs)
  , map (\(a, b) -> (fluents IMap.! a, fluents IMap.! b)) ms
  )
 where
  (fs, ms)              = mutex prob
  prob@Prob { fluents } = probify inp

makeMCs :: Ord vert => [vert] -> [(vert, vert)] -> [[[vert]]]
makeMCs fluents muts = map (retrieveMCLabels labels) (findCover g)
  where LabeledGraph g labels = mkGraph fluents muts

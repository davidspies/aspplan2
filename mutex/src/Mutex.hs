module Mutex
  ( Mutex
  , Prob(..)
  , mutex
  )
where

import           Debug.Trace                    ( trace )

import           IData.BiMap                    ( BiMap )
import qualified IData.BiMap                   as BiMap
import           IData.IMap                     ( IMap )
import qualified IData.IMap                    as IMap
import           IData.ISet                     ( ISet )
import qualified IData.ISet                    as ISet
import           Mutex.Act                      ( ActionId
                                                , Action(..)
                                                )
import qualified Mutex.Act                     as Act
import           Mutex.Fluent                   ( FluentId )

data Prob act fluent = Prob
  { fluents :: IMap FluentId fluent
  , actions :: IMap ActionId (Action act)
  , inits :: ISet FluentId
  }
  deriving Show

data Inverted = Inverted
  { invPre :: BiMap FluentId ActionId
  , invAdd :: BiMap FluentId ActionId
  }

inversions :: forall act fluent . Prob act fluent -> Inverted
inversions Prob { fluents, actions } = Inverted
  { invPre = invert Act.pre
  , invAdd = invert Act.add
  }
 where
  invert :: (Action act -> ISet FluentId) -> BiMap FluentId ActionId
  invert f =
    BiMap.pickRows (IMap.keysSet fluents) (BiMap.transpose $ IMap.map f actions)

type Mutex = (FluentId, FluentId)

data State = State
  { validFluents :: !(ISet FluentId)
  , mutexFF :: !(BiMap FluentId FluentId)
  }
  deriving (Eq)

start :: ISet FluentId -> State
start fs = State fs BiMap.empty

converge :: Eq a => (a -> a) -> a -> a
converge f = go (0 :: Int)
 where
  go count x =
    trace ("mutex round " ++ show count)
      $ let y = f x
        in  if x == y then trace "mutex finished" x else go (count + 1) y

mutex :: Prob act fluent -> (ISet FluentId, [Mutex])
mutex prob@Prob { fluents, actions, inits } =
  (validFluents result, BiMap.toList $ mutexFF result)
 where
  result        = converge go (start inits)
  adds          = IMap.map Act.add actions
  Inverted {..} = inversions prob
  go :: State -> State
  go State {..} = trace
    (  show fcount
    ++ " / "
    ++ show totcount
    ++ " fluents; "
    ++ show mcount
    ++ " constraints"
    )
    State {validFluents = newFluents, mutexFF = newFF}
   where
    newActs = ISet.fromList
      [ aid
      | (aid, Action { pre }) <- IMap.toList actions
      , pre `ISet.isSubsetOf` validFluents
      , BiMap.null
        $ IMap.map (`ISet.intersection` pre) (mutexFF `IMap.restrictKeys` pre)
      ]
    newFluents =
      ISet.unions $ validFluents : IMap.elems (IMap.restrictKeys adds newActs)
    diffFluents = newFluents ISet.\\ validFluents
    fcount      = ISet.size newFluents
    totcount    = IMap.size fluents
    mcount      = BiMap.count newFF `quot` 2
    newAF       = IMap.map
      (\Action { pre, add, del } ->
        ISet.unions
            (diffFluents : del : IMap.elems (mutexFF `IMap.restrictKeys` pre))
          ISet.\\ add
      )
      (IMap.restrictKeys actions newActs)
    newFA    = BiMap.pickRows newFluents $ BiMap.transpose newAF
    oneWayFF = IMap.mapWithKey
      (\k as ->
        let kAdds         = ISet.intersection as newActs
            kIsNew        = k `ISet.member` diffFluents
            possibleMutex = if kIsNew
              then ISet.delete k newFluents
              else (mutexFF BiMap.! k) `ISet.union` diffFluents
        in  ISet.fromList
              [ x
              | (x, xmut) <- IMap.toList $ IMap.restrictKeys newFA possibleMutex
              , kAdds `ISet.isSubsetOf` xmut
              ]
      )
      (IMap.restrictKeys invAdd newFluents)
    newFF = BiMap.intersection oneWayFF (BiMap.transpose oneWayFF)

module MCP.Solver.Stepless
  ( solve
  )
where

import           Clingo.Control                 ( Clingo
                                                , ClingoSetting(ClingoSetting)
                                                )
import qualified Clingo.Control                as Clingo
import           Clingo.Model
import qualified Clingo.Symbol                 as Clingo
import           Control.Monad                  ( forM )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import qualified Data.Text.Lazy                as LText
import           Debug.Trace                    ( traceShowM )
import           GHC.Stack                      ( HasCallStack )
import           System.FilePath                ( (</>) )

import           IData.Graph
import qualified IData.IMap                    as IMap
import           MCP.ASPInput
import           MCP.Config                     ( ASPFile(ASPFile) )
import           MCP.Solver.Output              ( actionStatement )
import           MCP.Util
import           PDDLParser.Translate.Shared    ( ReverseMap )

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

solve :: ASPInput -> ReverseMap -> IO LText.Text
solve inp rev = do
  aspPath <- getASPPath
  let
    go occs = do
      result <-
        Clingo.withClingo (ClingoSetting ["--opt-strategy=usc"] Nothing 0) $ do
          parts <- partsFrom occs
          setup
            (inp <> ASPInput [ASPFile $ aspPath </> "stepless.asp"] Nothing)
            parts
          Clingo.ground parts Nothing
          solveAwaitLastModel Nothing $ \m -> do
            useSuffix    <- Clingo.createFunction "useSuffix" [] True
            hasUseSuffix <- m `contains` useSuffix
            syms         <- modelSymbols m selectNone { selectShown = True }
            let pureSyms = map Clingo.toPureSymbol syms
            cost <- Cost <$> costVector m
            return (pureSyms, cost, hasUseSuffix)
      case result of
        Nothing                     -> return Nothing
        Just (syms, cost, suffUsed) -> do
          traceShowM cost
          if suffUsed then go (incrementOccs syms occs) else return (Just syms)
  go Map.empty <&> \case
    Nothing -> "No Solution"
    Just syms ->
      let
        LabeledGraph g labels = constructGraph syms
        actionSym             = \case
          Clingo.PureFunction "actOcc" [act, _] True -> Just act
          _ -> Nothing
        sortedVerts = mapMaybe (actionSym . (labels IMap.!)) $ topologicSort g
      in
        LText.unlines
          $ mapMaybe (fmap LText.fromStrict . actionStatement rev) sortedVerts

type OccurrenceMap = Map Clingo.PureSymbol Integer

partsFrom :: OccurrenceMap -> Clingo s [Clingo.Part s]
partsFrom m = do
  let allOccs = [ (k, i) | (k, v) <- Map.toList m, i <- [2 .. v] ]
  forM allOccs $ \(o, i) -> case o of
    Clingo.PureFunction "action" [act] True -> Clingo.Part "actOcc"
      <$> sequence [Clingo.unpureSymbol act, Clingo.createNumber i]
    Clingo.PureFunction "fluent" [flu] True -> Clingo.Part "fluentOcc"
      <$> sequence [Clingo.unpureSymbol flu, Clingo.createNumber i]
    _ -> error $ "Unknown saturation " ++ show o

incrementOccs :: [Clingo.PureSymbol] -> OccurrenceMap -> OccurrenceMap
incrementOccs syms = incrEach $ mapMaybe saturatedOccurrence syms
 where
  saturatedOccurrence = \case
    Clingo.PureFunction "saturated" [arg] True -> Just arg
    _ -> Nothing

-- Counter defaults to 1
incrEach :: Ord a => [a] -> Map a Integer -> Map a Integer
incrEach = Map.unionWith (\_ oldVal -> oldVal + 1) . Map.fromList . map (, 2)

constructGraph :: [Clingo.PureSymbol] -> LabeledGraph Clingo.PureSymbol
constructGraph syms = mkGraph verts edges
 where
  verts   = mapMaybe getVert syms
  getVert = \case
    Clingo.PureFunction "vertex" [vert] True -> Just vert
    _ -> Nothing
  inVerts   = Map.fromList $ mapMaybe getInVert syms
  getInVert = \case
    Clingo.PureFunction "inVertex" [x, y] True -> Just (x, y)
    _ -> Nothing
  edges   = mapMaybe getEdge syms
  getEdge = \case
    Clingo.PureFunction "edge" [x, y] True ->
      Just (inVerts ! x, inVerts ! y)
    _ -> Nothing

(!) :: HasCallStack => (Ord k, Show k) => Map k v -> k -> v
(!) m k = fromMaybe (error $ "Could not find key: " ++ show k) $ m Map.!? k

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
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as LText
import           Debug.Trace
import           GHC.Stack                      ( HasCallStack )
import           System.FilePath                ( (</>) )

import           IData.Graph
import qualified IData.IMap                    as IMap
import           MCP.ASPInput
import           MCP.Config
import           MCP.Solver.Output              ( actionStatement )
import           MCP.Util
import           PDDLParser.Translate.Shared    ( ReverseMap )

solve :: SteplessOptions -> ASPInput -> ReverseMap -> IO LText.Text
solve SteplessOptions { useUSC } inp rev = do
  aspPath <- getASPPath
  let
    go occs = do
      result <-
        Clingo.withClingo
            (ClingoSetting [ "--opt-strategy=usc" | useUSC ] Nothing 0)
          $ do
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
          if suffUsed
            then do
              traceM $ LText.unpack (buildSolution rev (Just syms))
              let sat = saturatedOccs syms
              go (incrEach sat occs)
            else return (Just syms)
  buildSolution rev <$> go Map.empty

buildSolution :: ReverseMap -> Maybe [Clingo.PureSymbol] -> LText.Text
buildSolution rev = \case
  Nothing -> "No Solution"
  Just syms ->
    let LabeledGraph g labels = constructGraph syms
        actionSym             = \case
          Normal (Clingo.PureFunction "actOcc" [act, _] True) ->
            Just (Normal act)
          Suffix (Clingo.PureFunction "happens" [act] True) ->
            Just (Suffix act)
          _ -> Nothing
        sortedVerts = mapMaybe (actionSym . (labels IMap.!)) $ topologicSort g
    in  LText.unlines
          $ mapMaybe (fmap LText.fromStrict . actionStatement' rev) sortedVerts

actionStatement' :: ReverseMap -> PlanVertex -> Maybe Text
actionStatement' rev = \case
  Normal x -> actionStatement rev x
  Suffix x -> ("suffix " <>) <$> actionStatement rev x

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

saturatedOccs :: [Clingo.PureSymbol] -> [Clingo.PureSymbol]
saturatedOccs = mapMaybe justIfSaturated
 where
  justIfSaturated = \case
    Clingo.PureFunction "saturated" [arg] True -> Just arg
    _ -> Nothing

-- Counter defaults to 1
incrEach :: Ord a => [a] -> Map a Integer -> Map a Integer
incrEach = Map.unionWith (\_ oldVal -> oldVal + 1) . Map.fromList . map (, 2)

data PlanVertex = Normal Clingo.PureSymbol | Suffix Clingo.PureSymbol
  deriving (Eq, Ord, Show)

constructGraph :: [Clingo.PureSymbol] -> LabeledGraph PlanVertex
constructGraph syms = mkGraph verts edges
 where
  verts   = mapMaybe getVert syms
  getVert = \case
    Clingo.PureFunction "vertex" [vert] True -> Just (Normal vert)
    Clingo.PureFunction "suffix" [vert@(Clingo.PureFunction "holds" _ True)] True
      -> Just (Suffix vert)
    Clingo.PureFunction "suffix" [vert@(Clingo.PureFunction "happens" _ True)] True
      -> Just (Suffix vert)
    _ -> Nothing
  inVerts   = Map.fromList $ mapMaybe getInVert syms
  getInVert = \case
    Clingo.PureFunction "inVertex" [x, y] True -> Just (Normal x, Normal y)
    _ -> Nothing
  edges   = mapMaybe getEdge syms
  getEdge = \case
    Clingo.PureFunction "edge" [x, y] True ->
      Just (inVerts ! Normal x, inVerts ! Normal y)
    Clingo.PureFunction "suffix" [Clingo.PureFunction "causes" [x, y] True] True
      -> Just
        ( Suffix (Clingo.PureFunction "happens" [x] True)
        , Suffix (Clingo.PureFunction "holds" [y] True)
        )
    Clingo.PureFunction "suffix" [Clingo.PureFunction "permits" [x, y] True] True
      -> Just
        ( Suffix (Clingo.PureFunction "holds" [x] True)
        , Suffix (Clingo.PureFunction "happens" [y] True)
        )
    Clingo.PureFunction "permits" [x, Clingo.PureFunction "subgoal" [y] True] True
      -> Just
        ( Normal (Clingo.PureFunction "holds" [x] True)
        , Suffix (Clingo.PureFunction "holds" [y] True)
        )
    _ -> Nothing

(!) :: HasCallStack => (Ord k, Show k) => Map k v -> k -> v
(!) m k = fromMaybe (error $ "Could not find key: " ++ show k) $ m Map.!? k

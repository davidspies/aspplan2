module MCP.Solver.Costs2
  ( solve
  )
where

import           Clingo.Control                as Clingo
                                         hiding ( solve )
import           Clingo.Model                   ( modelSymbols )
import qualified Clingo.Model                  as Model
import qualified Clingo.Solving                as Clingo
import qualified Clingo.Symbol                 as Clingo
import           Control.Concurrent             ( forkOS )
import           Control.Concurrent.Async       ( waitAny
                                                , withAsync
                                                )
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Text.Lazy                as LText
import           Debug.Trace                    ( traceM )
import           System.FilePath                ( (</>) )

import           MCP.ASPInput
import           MCP.Config                     ( ASPFile(ASPFile) )
import           MCP.Solver.Output
import           MCP.Util
import           PDDLParser.Translate.Shared    ( ReverseMap )

solve :: ASPInput -> ReverseMap -> IO LText.Text
solve inp rev = do
  sv  <- newSolutionVar
  res <- withAsync (variant1 inp sv)
    $ \v1 -> withAsync (variant2 inp sv) $ \v2 -> snd <$> waitAny [v1, v2]
  return $ formatSolution rev $ syms <$> res

data Model = Model
  { syms :: [Clingo.PureSymbol]
  , cost :: Cost
  }
  deriving Eq

data SolutionVar = SolutionVar
  { searching :: TVar MakeSpan
  , lowerBound :: TVar (Map MakeSpan Cost)
  , bestModel :: TVar (Maybe Model)
  }

newSolutionVar :: IO SolutionVar
newSolutionVar = atomically $ do
  lowerBound <- newTVar Map.empty
  searching  <- newTVar 0
  bestModel  <- newTVar Nothing
  return SolutionVar { .. }

bestLowerBound :: Map MakeSpan Cost -> MakeSpan -> Cost
bestLowerBound m makeSpan = maybe (Cost []) snd $ Map.lookupLE makeSpan m

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

checkResult :: SolutionVar -> ExceptT (Maybe Model) STM ()
checkResult SolutionVar {..} = ExceptT $ do
  makeSpan <- readTVar searching
  lb       <- readTVar lowerBound
  readTVar bestModel <&> \case
    Just m@Model { cost = ub } | bestLowerBound lb makeSpan >= ub ->
      Left (Just m)
    Nothing | bestLowerBound lb makeSpan == Infinity -> Left Nothing
    _ -> Right ()

newLowerBound :: MakeSpan -> Cost -> SolutionVar -> ExceptT (Maybe Model) IO ()
newLowerBound makeSpan cost sv@SolutionVar { lowerBound } =
  ExceptT $ atomically $ do
    lowerBound' <- readTVar lowerBound
    when (cost > bestLowerBound lowerBound' makeSpan) $ do
      traceM $ "Variant 2: Step " ++ show makeSpan ++ " " ++ show cost
      writeTVar lowerBound (Map.insert makeSpan cost lowerBound')
    runExceptT $ checkResult sv

newBestModel :: Model -> SolutionVar -> ExceptT (Maybe Model) IO ()
newBestModel m sv@SolutionVar { bestModel } = ExceptT $ atomically $ do
  current <- readTVar bestModel
  when (maybe True ((cost m <) . cost) current) $ do
    let Model { cost } = m
    traceM $ "Variant 1: " ++ show cost
    writeTVar bestModel (Just m)
  runExceptT $ checkResult sv

searchingNewLayer :: MakeSpan -> SolutionVar -> ExceptT (Maybe Model) IO ()
searchingNewLayer makeSpan sv@SolutionVar { searching } =
  ExceptT $ atomically $ do
    current <- readTVar searching
    when (makeSpan > current) $ do
      traceM $ "Variant 1: Step " ++ show makeSpan
      writeTVar searching makeSpan
    runExceptT $ checkResult sv

maybeCatch :: Functor m => ExceptT err m () -> m (Maybe err)
maybeCatch act = either Just (\() -> Nothing) <$> runExceptT act

-- Doesn't block when killed (but leaves the action running)
withClingoThread :: ClingoSetting -> (forall s . ClingoT IO s r) -> IO r
withClingoThread setting act = do
  resultRef <- newEmptyTMVarIO
  _         <- forkOS
    (withClingo setting (liftIO . atomically . putTMVar resultRef =<< act))
  atomically (takeTMVar resultRef)

variant1 :: ASPInput -> SolutionVar -> IO (Maybe Model)
variant1 inp sv = do
  aspPath <- getASPPath
  withClingoThread defaultClingo $ do
    setup (inp <> ASPInput [ASPFile $ aspPath </> "aspplan.asp"] Nothing) []
    stepping $ \makeSpan -> maybeCatch $ do
      mapExceptT liftIO $ searchingNewLayer makeSpan sv
      ExceptT $ Clingo.withSolver [] $ \solver -> runExceptT $ do
        let go = lift (waitUntilModel solver) >>= \case
              Nothing -> return ()
              Just m  -> do
                syms <- lift $ map Clingo.toPureSymbol <$> modelSymbols
                  m
                  Model.selectNone { Model.selectShown = True }
                cost <- lift $ Cost <$> Model.costVector m
                mapExceptT liftIO $ newBestModel Model { .. } sv
                lift $ Clingo.solverResume solver
                go
        go

variant2 :: ASPInput -> SolutionVar -> IO (Maybe Model)
variant2 inp sv@SolutionVar { lowerBound } = do
  aspPath <- getASPPath
  withClingoThread (ClingoSetting [] Nothing 0) $ do
    setup
      (inp <> ASPInput [ASPFile $ aspPath </> "aspplan_suffix.asp"] Nothing)
      []
    stepping $ \makeSpan -> do
      traceM $ "Variant 2: Step " ++ show makeSpan
      lb  <- liftIO $ readTVarIO lowerBound
      res <- solveAwaitLastModel (Just $ bestLowerBound lb makeSpan) $ \m -> do
        useSuffix    <- Clingo.createFunction "useSuffix" [] True
        hasUseSuffix <- m `Model.contains` useSuffix
        cost         <- Cost <$> Model.costVector m
        if hasUseSuffix
          then return $ Left cost
          else do
            syms <- map Clingo.toPureSymbol
              <$> modelSymbols m Model.selectNone { Model.selectShown = True }
            return $ Right (syms, cost)
      liftIO $ maybeCatch $ case res of
        Nothing                   -> newLowerBound makeSpan Infinity sv
        Just (Left  cost        ) -> newLowerBound makeSpan cost sv
        Just (Right (syms, cost)) -> do
          traceM $ "Variant 2: Found Solution: " ++ show cost
          throwError $ Just Model { .. }

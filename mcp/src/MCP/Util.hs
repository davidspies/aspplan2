module MCP.Util
  ( Cost(..)
  , MakeSpan
  , getASPPath
  , solveAwaitLastModel
  , stepping
  , waitUntilModel
  )
where

import           Clingo.Control                 ( Clingo )
import qualified Clingo.Control                as Clingo
import           Clingo.Model
import qualified Clingo.Solving                as Clingo
import qualified Clingo.Symbol                 as Clingo
import           Control.Monad                  ( when )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           System.Environment             ( lookupEnv )

firstJustM :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
firstJustM xs0 act = go xs0
 where
  go = \case
    []       -> return Nothing
    (x : xs) -> act x >>= \case
      Just res -> return $ Just res
      Nothing  -> go xs

type MakeSpan = Integer

stepping :: (MakeSpan -> Clingo s (Maybe b)) -> Clingo s b
stepping act = fmap fromJust $ firstJustM [(0 :: MakeSpan) ..] $ \i -> do
  inum <- Clingo.createNumber i

  when (i > 0) $ do
    Clingo.ground [Clingo.Part "step" [inum]] Nothing

    prevNum   <- Clingo.createNumber (i - 1)
    queryPrev <- Clingo.createFunction "query" [prevNum] True
    Clingo.releaseExternal queryPrev

  Clingo.cleanup

  queryNow <- Clingo.createFunction "query" [inum] True
  Clingo.assignExternal queryNow Clingo.TruthTrue

  act i

data Cost = Cost [Integer] | Infinity
  deriving (Eq, Ord, Show)

solveAwaitLastModel
  :: forall s a
   . Maybe Cost
  -> (forall s1 . Model s1 -> Clingo.IOSym s1 a)
  -> Clingo s (Maybe a)
solveAwaitLastModel stoppingCost f =
  Clingo.withSolver [] $ \(solver :: Clingo.Solver s1) -> do
    let go :: Maybe a -> Clingo.IOSym s1 (Maybe a)
        go previous = waitUntilModel solver >>= \case
          Nothing  -> return previous
          Just res -> do
            n    <- f res
            cost <- Cost <$> costVector res
            if maybe False (cost <=) stoppingCost
              then return (Just n)
              else do
                Clingo.solverResume solver
                go $ Just n
    go Nothing

waitUntilModel :: Clingo.Solver s -> Clingo.IOSym s (Maybe (Model s))
waitUntilModel solver = do
  let awaitResult = Clingo.solverWait solver (-1) >>= \case
        Clingo.Ready    -> return ()
        Clingo.NotReady -> awaitResult
  awaitResult
  Clingo.getModel solver

getASPPath :: IO FilePath
getASPPath = fromMaybe "asp" <$> lookupEnv "ASP_PATH"

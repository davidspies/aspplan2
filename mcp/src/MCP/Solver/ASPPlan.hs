module MCP.Solver.ASPPlan
  ( solve
  )
where

import qualified Clingo.Control                as Clingo
import           Clingo.Model
import qualified Clingo.Symbol                 as Clingo
import           Control.Monad                  ( (>=>) )
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
  aspPath <- getASPPath
  res     <- Clingo.withDefaultClingo $ do
    setup (inp <> ASPInput [ASPFile $ aspPath </> "aspplan.asp"] Nothing) []
    stepping $ \makeSpan -> do
      traceM $ "Step " ++ show makeSpan
      Clingo.withSolver [] $ waitUntilModel >=> \case
        Nothing -> return Nothing
        Just m  -> do
          syms <- modelSymbols m selectNone { selectShown = True }
          return $ Just $ map Clingo.toPureSymbol syms
  return $ formatSolution rev (Just res)

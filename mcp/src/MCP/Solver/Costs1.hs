module MCP.Solver.Costs1
  ( solve
  )
where

import           Clingo.Control                 ( ClingoSetting(ClingoSetting) )
import qualified Clingo.Control                as Clingo
import           Clingo.Model
import qualified Clingo.Symbol                 as Clingo
import qualified Data.Text.Lazy                as LText
import           Debug.Trace                    ( traceM
                                                , traceShowM
                                                )
import           System.FilePath                ( (</>) )

import           MCP.ASPInput
import           MCP.Config                     ( ASPFile(ASPFile) )
import           MCP.Solver.Output
import           MCP.Util
import           PDDLParser.Translate.Shared    ( ReverseMap )

solve :: ASPInput -> ReverseMap -> IO LText.Text
solve inp rev = do
  aspPath <- getASPPath
  Clingo.withClingo (ClingoSetting [] Nothing 0) $ do
    setup
      (inp <> ASPInput [ASPFile $ aspPath </> "aspplan_suffix.asp"] Nothing)
      []
    stepping $ \makeSpan -> do
      traceM $ "Step " ++ show makeSpan
      res <- solveAwaitLastModel Nothing $ \m -> do
        useSuffix    <- Clingo.createFunction "useSuffix" [] True
        hasUseSuffix <- m `contains` useSuffix
        syms         <- modelSymbols m selectNone { selectShown = True }
        let pureSyms = map Clingo.toPureSymbol syms
        cost <- Cost <$> costVector m
        return (pureSyms, cost, hasUseSuffix)
      case res of
        Nothing -> return $ Just $ formatSolution rev Nothing
        Just (syms, cost, suffUsed) -> do
          traceShowM cost
          return $ if suffUsed
            then Nothing
            else Just $ formatSolution rev (Just syms)

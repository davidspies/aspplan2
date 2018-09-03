module Main where

import qualified Clingo.Control                as Clingo
import           Clingo.Model
import qualified Clingo.Solving                as Clingo
import           Clingo.Symbol                  ( PureSymbol(..) )
import qualified Clingo.Symbol                 as Clingo
import           Control.Exception              ( evaluate )
import           Control.DeepSeq                ( force )
import           Control.Monad
import           Data.Bifunctor                 ( first )
import qualified Data.ByteString               as BS
import qualified Data.Text.IO                  as Text
import qualified Data.Text.Lazy                as LText
import qualified Data.Text.Lazy.IO             as LText
import qualified Data.Yaml                     as Yaml
import           Debug.Trace                    ( traceM )
import           System.Environment             ( getArgs )
import           System.FilePath                ( (</>) )
import           System.IO
import qualified Text.PrettyPrint.Leijen.Text  as PP

import           MCP.ASPInput
import           MCP.Config
import           MCP.Rules                      ( docRules )
import qualified MCP.Solver.ASPPlan            as ASPPlan
import qualified MCP.Solver.Costs1             as Costs1
import qualified MCP.Solver.Costs2             as Costs2
import qualified MCP.Solver.Stepless           as Stepless
import           MCP.Util                       ( getASPPath )
import           Mutex.Prob
import qualified PDDLParser.Parse.Domain       as Parse
import qualified PDDLParser.Parse.Problem      as Parse
import           PDDLParser.Parse.Shared        ( parseOrErr )
import qualified PDDLParser.PDDL               as PDDL
import qualified PDDLParser.Translate.Domain   as Domain
                                                ( translate )
import qualified PDDLParser.Translate.Problem  as Problem
                                                ( translate )
import           PDDLParser.Translate.Shared    ( ReverseMap )

main :: IO ()
main = do
  [cfgFileName] <- getArgs
  cfg           <- Yaml.decodeThrow =<< BS.readFile cfgFileName
  case cfg of
    ParseDomain  domain        -> runParseDomain domain
    ParseProblem problem       -> runParseProblem problem
    Parse        domainProblem -> runParseDomainProblem domainProblem
    Ground       grounded      -> runGround grounded
    Solve        pipeline      -> runSolve pipeline

parseDomain :: Domain -> IO PDDL.Domain
parseDomain = \case
  PDDLDomain path -> Parse.parseDomain path <$> LText.readFile path
  StdinDomain     -> Parse.parseDomain "(stdin)" <$> LText.getContents

parseProblem :: Maybe PDDL.Domain -> Problem -> IO PDDL.Problem
parseProblem md = \case
  PDDLProblem path -> Parse.parseProb md path <$> LText.readFile path
  StdinProblem     -> Parse.parseProb md "(stdin)" <$> LText.getContents

runParseDomain :: Domain -> IO ()
runParseDomain = LText.putStrLn . fst . Domain.translate <=< parseDomain

runParseProblem :: Problem -> IO ()
runParseProblem =
  LText.putStrLn . fst . Problem.translate <=< parseProblem Nothing

parseDomainProblem :: DomainProblem -> IO (ASPInput, ReverseMap)
parseDomainProblem dp = case dp of
  DomainProblem (From StdinDomain) (From StdinProblem) -> do
    (d, p) <-
      parseOrErr
          (do
            d <- Parse.domain
            p <- Parse.problem (Just d)
            return (d, p)
          )
          "(stdin)"
        <$> LText.getContents
    let (d', rd) = Domain.translate d
        (p', rp) = Problem.translate p
    return (ASPInput [] (Just . LText.toStrict $ d' <> p'), rd <> rp)
  DomainProblem { domain, problem } -> do
    d <- case domain of
      From x        -> Left <$> parseDomain x
      Stdin         -> Right . ASPInput [] . Just <$> Text.getContents
      InputFiles fs -> pure $ Right (ASPInput fs Nothing)
    p <- case problem of
      From x        -> Left <$> parseProblem (either Just (const Nothing) d) x
      Stdin         -> Right . ASPInput [] . Just <$> Text.getContents
      InputFiles fs -> pure $ Right (ASPInput fs Nothing)
    let d' = either
          (first (ASPInput [] . Just . LText.toStrict) . Domain.translate)
          (, mempty)
          d
    let p' = either
          (first (ASPInput [] . Just . LText.toStrict) . Problem.translate)
          (, mempty)
          p
    return $ d' <> p'

runParseDomainProblem :: DomainProblem -> IO ()
runParseDomainProblem dp = do
  (ASPInput _ t, _) <- parseDomainProblem dp
  case t of
    Nothing -> error "All inputs already ASP"
    Just t' -> Text.putStrLn t'

groundSymbols :: Grounded -> IO ([PureSymbol], ReverseMap)
groundSymbols Grounded { input, mutexType } = do
  aspPath <- getASPPath
  let buildasp = [ASPFile $ aspPath </> "build.asp"]
  (grounded, rev) <- case input of
    Stdin -> do
      t <- Text.getContents
      (, mempty) <$> runClingoBase (ASPInput buildasp (Just t))
    InputFiles fs ->
      (, mempty) <$> runClingoBase (ASPInput (buildasp ++ fs) Nothing)
    From dp -> do
      (inp, rev) <- parseDomainProblem dp
      (, rev) <$> runClingoBase (inp <> ASPInput buildasp Nothing)
  let newGrounded = mutexHandling mutexType grounded
  pure (newGrounded, rev)

runGround :: Grounded -> IO ()
runGround grounded = do
  (descr, _) <- groundSymbols grounded
  PP.displayIO stdout $ docRules descr

runSolve :: Pipeline -> IO ()
runSolve Pipeline { solver, grounded } = do
  (descrU, rev) <- case grounded of
    Stdin         -> (, mempty) . ASPInput [] . Just <$> Text.getContents
    InputFiles fs -> pure (ASPInput fs Nothing, mempty)
    From       x  -> do
      (g, rev) <- groundSymbols x
      return
        (ASPInput [] . Just . LText.toStrict . PP.displayT $ docRules g, rev)
  descr <- evaluate $ force descrU
  traceM "Begin solving"
  case solver of
    ASPPlan     -> ASPPlan.solve descr rev >>= putStrLnAndClose
    CostsSingle -> Costs1.solve descr rev >>= putStrLnAndClose
    CostsDouble -> Costs2.solve descr rev putStrLnAndClose
    Stepless    -> Stepless.solve descr rev >>= putStrLnAndClose

putStrLnAndClose :: LText.Text -> IO ()
putStrLnAndClose s = do
  LText.putStrLn s
  hClose stdout

mutexHandling :: MutexType -> [PureSymbol] -> [PureSymbol]
mutexHandling mt syms = case mt of
  None         -> syms
  Direct       -> symsModified ++ map mutexRule muts
  Multicliques -> symsModified ++ mcs
 where
  (newFluents, muts) = genMutex syms
  isFluent           = \case
    PureFunction "fluent" [_] True -> True
    _                              -> False
  mkFluent x = PureFunction "fluent" [x] True
  symsModified = map mkFluent newFluents ++ filter (not . isFluent) syms
  mcs          = mcRules (makeMCs newFluents muts)

runClingoBase :: ASPInput -> IO [PureSymbol]
runClingoBase inp = Clingo.withDefaultClingo $ do
  setup inp []
  Clingo.withSolver [] $ \solver -> do
    let awaitResult = Clingo.solverWait solver (-1) >>= \case
          Clingo.Ready    -> return ()
          Clingo.NotReady -> awaitResult
    awaitResult
    Clingo.getModel solver >>= \case
      Nothing -> return $ error "No solution"
      Just m  -> do
        syms <- modelSymbols m selectNone { selectShown = True }
        return $ map Clingo.toPureSymbol syms

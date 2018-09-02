module Main where

import           System.Environment             ( getArgs )
import           System.IO
import qualified Data.Text.Lazy.IO             as Text

import           PDDLParser.Parse.Domain        ( parseDomain )
import           PDDLParser.Parse.Problem       ( parseProb )
import qualified PDDLParser.Translate.Domain   as Domain
import qualified PDDLParser.Translate.Problem  as Prob

main :: IO ()
main = do
  [domainFileName, probFileName] <- getArgs
  domainText <- Text.hGetContents =<< openFile domainFileName ReadMode
  let d = parseDomain domainFileName domainText
  probText <- Text.hGetContents =<< openFile probFileName ReadMode
  let p = parseProb (Just d) probFileName probText
  Text.putStrLn $ fst $ Domain.translate d
  Text.putStrLn $ fst $ Prob.translate p

module MCP.Solver.Output
  ( actionStatement
  , formatSolution
  )
where

import           Clingo.Symbol                  ( PureSymbol(..) )
import           Data.List                      ( sortOn )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as LText

import qualified PDDLParser.PDDL               as PDDL
import           PDDLParser.Translate.Shared    ( ReverseMap(..) )

formatSolution :: ReverseMap -> Maybe [PureSymbol] -> LText.Text
formatSolution rev res = case res of
  Nothing -> "No solution"
  Just r ->
    let layerAct = \case
          PureFunction "do" [arg, PureNumber t] True ->
            (, t) <$> actionStatement rev arg
          _ -> Nothing
    in  LText.unlines
          $ map (LText.fromStrict . fst) (sortOn snd $ mapMaybe layerAct r)

actionStatement :: ReverseMap -> PureSymbol -> Maybe Text
actionStatement ReverseMap {..} = \case
  PureFunction actName actArgs True ->
    let aname = maybe actName PDDL.unName (Map.lookup actName actions)
        objnames =
          [ maybe x PDDL.unName (objects Map.!? x)
          | PureFunction x [] _ <- actArgs
          ]
    in  Just $ "(" <> Text.unwords (aname : objnames) <> ")"
  _ -> Nothing

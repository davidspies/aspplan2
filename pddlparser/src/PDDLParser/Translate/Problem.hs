module PDDLParser.Translate.Problem
  ( translate
  )
where

import qualified Data.Map                      as Map
import qualified Data.Text.Lazy                as LText
import           Data.Text                      ( Text )

import           PDDLParser.ASP
import           PDDLParser.PDDL
import           PDDLParser.Translate.Shared

translate :: Problem -> (LText.Text, ReverseMap)
translate Problem {..} =
  ( LText.unlines
    $  map (LText.fromStrict . stringRule)
    $  concatMap objectLine objects
    ++ map initLine inits
    ++ map goalLine goals
    ++ [ optimLine | optimize ]
  , ReverseMap Map.empty (buildReverse $ map typedValue objects)
  )

initLine :: Init -> Rule
initLine = \case
  InitFluent f -> formulaLine "initp" f
  FunctionValue f (Number val) ->
    H (P "value" [formulaObj deident f, v val]) :- []

goalLine :: Formula Name -> Rule
goalLine = formulaLine "goalp"

formulaLine :: Text -> Formula Name -> Rule
formulaLine n f = H (P n [formulaObj deident f]) :- []

optimLine :: Rule
optimLine = H (v "minimizeTotalCost") :- []

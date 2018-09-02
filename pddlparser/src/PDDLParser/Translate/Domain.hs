module PDDLParser.Translate.Domain
  ( translate
  )
where

import           Data.Char
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as LText

import           PDDLParser.ASP
import           PDDLParser.PDDL
import           PDDLParser.Translate.Shared

translate :: Domain -> (LText.Text, ReverseMap)
translate Domain {..} =
  ( LText.unlines
    $  map (LText.fromStrict . stringRule)
    $  concatMap typeLine    types
    ++ concatMap objectLine  constants
    ++ concatMap actionLines actions
  , ReverseMap (buildReverse $ map actionName actions)
               (buildReverse $ map typedValue constants)
  )
  where actionName Action { name = actName } = actName

typeLine :: Typed Name -> [Rule]
typeLine = \case
  Typed x t ->
    [ H (P "isSubtype" [v (deident x), v (deident t')]) :- [] | t' <- t ]
  Untyped{} -> []

actionLines :: Action -> [Rule]
actionLines Action {..} =
  (  H (P "actionp" [actionExpr])
    :- mapMaybe matchTypes parameters
    ++ map fluentPre preconditions
    )
    :  map (relates "prep") preconditions
    ++ zipWith effectLine [0 ..] effects
 where
  actionExpr = p (deident name) (map (devar . unt) parameters)
  effectLine :: Int -> Effect -> Rule
  effectLine effectId = \case
    Add f -> relates "addp" f
    Del f -> relates "delp" f
    Cost n ->
      H
          (P
            "costp"
            [ actionExpr
            , v (Text.pack $ show effectId)
            , case n of
              NumConst (Number c) -> p "constValue" [c]
              NumVar   f          -> P "valueOf" [formulaObj determ f]
            ]
          )
        :- actionBody
  relates :: Text -> Formula Term -> Rule
  relates s f = H (P s [actionExpr, formulaObj determ f]) :- actionBody
  actionBody :: [BodyExpr]
  actionBody = [B (P "actionp" [actionExpr])]

fluentPre :: Formula Term -> BodyExpr
fluentPre f = B $ P "fluentp" [formulaObj determ f]

matchTypes :: Typed Variable -> Maybe BodyExpr
matchTypes = \case
  Untyped{}  -> Nothing
  Typed x ts -> case ts of
    []        -> error "no types!?"
    [     t ] -> Just $ B $ p "hasType" [devar x, deident t]
    _ : _ : _ -> Just $ Disj $ map (\t -> p "hasType" [devar x, deident t]) ts

devar :: Variable -> Text
devar (Variable (Name n)) = Text.map (toUpper . replaceDash) n

determ :: Term -> Text
determ = \case
  TName n   -> deident n
  TVar  var -> devar var

unt :: Typed t -> t
unt = \case
  Untyped x -> x
  Typed x _ -> x

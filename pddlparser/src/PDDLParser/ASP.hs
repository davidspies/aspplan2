module PDDLParser.ASP
  ( BodyExpr(..)
  , Expr(..)
  , HeadExpr(..)
  , Rule(..)
  , stringRule
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

data Rule = HeadExpr :- [BodyExpr]
infix 1 :-

data HeadExpr = Empty | H Expr
data BodyExpr = B Expr | Not Expr | Disj [Expr]
data Expr = P Text [Expr]

stringRule :: Rule -> Text
stringRule (h :- b) = case (h, b) of
  (Empty, []) -> error "Empty rule"
  (H h' , []) -> stringExpr h' <> "."
  (_, _ : _) ->
    (case h of
        Empty -> ""
        H h'  -> stringExpr h'
      )
      <> ":-"
      <> Text.intercalate ";" (map stringBodyExpr b)
      <> "."

stringExpr :: Expr -> Text
stringExpr (P n args) = case args of
  []      -> n
  (_ : _) -> n <> "(" <> Text.intercalate "," (map stringExpr args) <> ")"

stringBodyExpr :: BodyExpr -> Text
stringBodyExpr = \case
  B    e  -> stringExpr e
  Not  e  -> "not " <> stringExpr e
  Disj es -> "{" <> Text.intercalate ";" (map stringExpr es) <> "} >= 1."

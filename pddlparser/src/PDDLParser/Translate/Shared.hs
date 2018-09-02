module PDDLParser.Translate.Shared
  ( ReverseMap(..)
  , buildReverse
  , deident
  , formulaObj
  , objectLine
  , p
  , v
  , replaceDash
  , typedValue
  )
where

import           Data.Char                      ( toLower )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )

import           PDDLParser.ASP
import           PDDLParser.PDDL

data ReverseMap = ReverseMap
  { actions :: Map Text Name
  , objects :: Map Text Name
  }

instance Semigroup ReverseMap where
  (<>) (ReverseMap x1 y1) (ReverseMap x2 y2) = ReverseMap (x1 <> x2) (y1 <> y2)
instance Monoid ReverseMap where
  mempty = ReverseMap mempty mempty

buildReverse :: [Name] -> Map Text Name
buildReverse ns = Map.fromList [ (deident n, n) | n <- ns ]

objectLine :: Typed Name -> [Rule]
objectLine = \case
  Untyped{} -> []
  (Typed c ts) ->
    (`map` ts) $ \t -> H (p "hasType" [deident c, deident t]) :- []

p :: Text -> [Text] -> Expr
p n vs = P n (map v vs)

v :: Text -> Expr
v = (`P` [])

deident :: Name -> Text
deident (Name n) = Text.map (toLower . replaceDash) n

replaceDash :: Char -> Char
replaceDash = \case
  '-' -> '_'
  c   -> c

formulaObj :: (t -> Text) -> Formula t -> Expr
formulaObj d (Formula n args) = p (deident n) (map d args)

typedValue :: Typed v -> v
typedValue = \case
  Typed obj _ -> obj
  Untyped obj -> obj

module PDDLParser.PDDL
  ( Action(..)
  , Domain(..)
  , Effect(..)
  , Formula(..)
  , Function(..)
  , Init(..)
  , Name(..)
  , Number(..)
  , NumericFormula(..)
  , Predicate(..)
  , Problem(..)
  , Requirement(..)
  , Term(..)
  , Typed(..)
  , Variable(..)
  , supported
  )
where

import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

data Requirement
  = Strips
  | Typing
  | NegativePreconditions
  | DisjunctivePreconditions
  | Equality
  | ExistentialPreconditions
  | UniversalPreconditions
  | QuantifiedPreconditions
  | ConditionalEffects
  | Fluents
  | NumericFluents
  | ADL
  | DurativeActions
  | DurationInequalities
  | ContinuousEffects
  | DerivedPredicates
  | TimedInitialLiterals
  | Preferences
  | Constraints
  | ActionCosts
  deriving (Eq, Read, Show, Bounded, Enum)

supported :: [Requirement]
supported = [Strips, Typing, ActionCosts]

data Predicate = Predicate Name [Typed Variable]
  deriving Show
newtype Variable = Variable Name

data Typed x = Typed x [Name] | Untyped x
  deriving Show

instance Show Variable where
  show (Variable n) = '?' : show n

data Action = Action
  { name :: Name
  , parameters :: [Typed Variable]
  , preconditions :: [Formula Term]
  , effects :: [Effect]
  }
  deriving Show
data Formula t = Formula Name [t]
  deriving Show
data Term = TName Name | TVar Variable
instance Show Term where
  show = \case
    TName n -> show n
    TVar v -> show v
data Effect = Add (Formula Term) | Del (Formula Term) | Cost NumericFormula
  deriving Show
data Problem = Problem
  { name :: Name
  , objects :: [Typed Name]
  , inits :: [Init]
  , goals :: [Formula Name]
  , optimize :: Bool
  }
  deriving Show

data NumericFormula = NumConst Number | NumVar (Formula Term)
  deriving Show

data Init = InitFluent (Formula Name) | FunctionValue (Formula Name) Number
  deriving Show

newtype Number = Number Text
instance Show Number where
  show (Number n) = Text.unpack n

data Domain = Domain
  { name :: Name
  , requirements :: [Requirement]
  , types :: [Typed Name]
  , constants :: [Typed Name]
  , predicates :: [Predicate]
  , functions :: [Function Variable]
  , actions :: [Action]
  }
  deriving (Show)

newtype Name = Name {unName :: Text}
  deriving (Eq)
  deriving newtype (IsString)

instance Show Name where
  show (Name n) = Text.unpack n

data Function var = Function
  { name :: Name
  , parameters :: [Typed var]
  }
  deriving (Show)

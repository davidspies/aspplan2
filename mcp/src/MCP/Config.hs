module MCP.Config where

import           Control.Category               ( (>>>) )
import           Control.DeepSeq                ( NFData )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Aeson                    as JSON
import           GHC.Generics                   ( Generic )

data Solver = ASPPlan | CostsSingle | CostsDouble | Stepless
  deriving (Show, Generic, FromJSON, ToJSON)

data MutexType = None | Direct | Multicliques
  deriving (Show, Generic, FromJSON, ToJSON)

data Pipeline = Pipeline
  { solver :: Solver
  , grounded :: ASPInputSpec Grounded
  }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype ASPFile = ASPFile {unASPFile :: FilePath}
  deriving newtype (Show, FromJSON, ToJSON, NFData)

data ASPInputSpec x = Stdin | InputFiles [ASPFile] | From x
  deriving (Show)

instance FromJSON x => FromJSON (ASPInputSpec x) where
  parseJSON val = case JSON.fromJSON val of
    JSON.Success r -> case recAsInput r of
      Nothing -> asVal
      Just x -> pure x
    JSON.Error{} -> asVal
    where
      asVal = From <$> JSON.parseJSON val

instance ToJSON x => ToJSON (ASPInputSpec x) where
  toJSON = inputAsRec >>> \case
    Left r -> JSON.toJSON r
    Right v -> JSON.toJSON v
  toEncoding = inputAsRec >>> \case
    Left r -> JSON.toEncoding r
    Right v -> JSON.toEncoding v

inputAsRec :: ASPInputSpec x -> Either ASPInputRec x
inputAsRec = \case
  Stdin         -> Left $ ASPInputRec (Just True) Nothing
  InputFiles fs -> Left $ ASPInputRec Nothing (Just fs)
  From       x  -> Right x

recAsInput :: ASPInputRec -> Maybe (ASPInputSpec x)
recAsInput = \case
  ASPInputRec { stdin = Just True } -> Just Stdin
  ASPInputRec { files = Just fs@(_ : _) } -> Just $ InputFiles fs
  _ -> Nothing

data ASPInputRec = ASPInputRec
  { stdin :: Maybe Bool
  , files :: Maybe [ASPFile]
  }
  deriving (Generic)
instance FromJSON ASPInputRec where
  parseJSON =
    JSON.genericParseJSON JSON.defaultOptions{JSON.omitNothingFields = True}
instance ToJSON ASPInputRec where
  toJSON = JSON.genericToJSON JSON.defaultOptions{JSON.omitNothingFields = True}
  toEncoding =
    JSON.genericToEncoding JSON.defaultOptions{JSON.omitNothingFields = True}

data Grounded = Grounded
  { input :: ASPInputSpec DomainProblem
  , mutexType :: MutexType
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data DomainProblem
  = DomainProblem {domain :: ASPInputSpec Domain, problem :: ASPInputSpec Problem}
  deriving (Show, Generic, FromJSON, ToJSON)

data Domain = PDDLDomain FilePath | StdinDomain
  deriving (Show, Generic, FromJSON, ToJSON)

data Problem = PDDLProblem FilePath | StdinProblem
  deriving (Show, Generic, FromJSON, ToJSON)

data Config
  = ParseDomain Domain
  | ParseProblem Problem
  | Parse DomainProblem
  | Ground{grounded :: Grounded, mutexOnly :: Bool}
  | Solve Pipeline
  deriving (Show, Generic, FromJSON, ToJSON)

module MCP.ASPInput
  ( ASPInput(..)
  , setup
  )
where

import           Clingo.Control                 ( Clingo )
import qualified Clingo.Control                as Clingo
import           Control.Monad                  ( forM_ )
import           Control.DeepSeq                ( NFData )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

import           MCP.Config                     ( ASPFile(..) )

data ASPInput = ASPInput
  { files :: [ASPFile]
  , text :: Maybe Text
  }
  deriving (Show,Generic,NFData)

instance Semigroup ASPInput where
  (<>) (ASPInput x1 y1) (ASPInput x2 y2) = ASPInput (x1 <> x2) (y1 <> y2)

setup :: ASPInput -> [Clingo.Part s] -> Clingo s ()
setup (ASPInput inps progin) parts = do
  mapM_ (Clingo.loadProgram . unASPFile) inps
  forM_ progin $ Clingo.addProgram "extra" []
  Clingo.ground ([Clingo.Part "base" [], Clingo.Part "extra" []] ++ parts)
                Nothing

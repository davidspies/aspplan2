module PDDLParser.Parse.Problem
  ( problem
  , parseProb
  )
where

import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as Text
import           Data.Text.Lazy                 ( Text )
import           Prelude                 hiding ( init )

import           Control.Applicative            ( (<|>) )
import           Control.Monad
import           Text.Parsec.Text.Lazy          ( Parser )

import           PDDLParser.Parse.Shared
import           PDDLParser.PDDL

ignoreCaseMatches :: Name -> Name -> Bool
ignoreCaseMatches (Name x) (Name y) = Text.toLower x == Text.toLower y

parseProb :: Maybe Domain -> String -> Text -> Problem
parseProb d = parseOrErr (problem d)

problem :: Maybe Domain -> Parser Problem
problem d = do
  whiteSpace
  parens $ do
    void $ string "define"
    probName <- parens $ do
      void $ string "problem"
      identifier
    parens $ do
      void $ string ":domain"
      domainName' <- identifier
      case d of
        Nothing -> pure ()
        Just Domain { name = domainName } ->
          unless (domainName `ignoreCaseMatches` domainName')
            $  fail
            $  "Wrong domain! "
            ++ show domainName
            ++ " does not match "
            ++ show domainName'
    objects  <- objects'
    inits    <- inits'
    goals    <- taggedListItem "goal" (parens (gd identifier))
    optimize <- isOptimize
    return Problem {name = probName, ..}

inits' :: Parser [Init]
inits' = taggedList "init" init

init :: Parser Init
init = parens $
    (do
      void $ trystring "="
      FunctionValue <$> parens (atomicFormula identifier) <*> number
    )
    <|> (InitFluent <$> atomicFormula identifier)

objects' :: Parser [Typed Name]
objects' = taggedListItem "objects" $ typedList identifier

isOptimize :: Parser Bool
isOptimize = fmap
  (fromMaybe False)
  (taggedItem "metric" $ do
    void $ string "minimize"
    void $ parens $ string "total-cost"
    return True
  )

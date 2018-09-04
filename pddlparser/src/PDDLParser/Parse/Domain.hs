module PDDLParser.Parse.Domain
  ( domain
  , parseDomain
  )
where

import           Data.Text.Lazy                 ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as LText
import           Control.Monad
import           Text.Parsec             hiding ( string )
import           Text.Parsec.Text.Lazy          ( Parser )

import           PDDLParser.Parse.Case          ( titleToDash )
import           PDDLParser.Parse.Shared
import           PDDLParser.PDDL

parseDomain :: String -> Text -> Domain
parseDomain = parseOrErr domain

domain :: Parser Domain
domain = do
  whiteSpace
  parens $ do
    void $ string "define"
    domainName <- parens $ do
      void $ string "domain"
      identifier
    requirements <- parseAndCheckRequirements
    types        <- types'
    constants    <- constants'
    predicates   <- predicates'
    functions    <- functions'
    actions      <- many action
    return Domain {name = domainName, ..}

types' :: Parser [Typed Name]
types' = taggedListItem "types" $ typedList identifier

parseAndCheckRequirements :: Parser [Requirement]
parseAndCheckRequirements = do
  requirements <- requirements'
  case filter (`notElem` supported) requirements of
    []    -> return requirements
    unsup -> fail $ "No support for requirements: " ++ show
      (map ((':' :) . Text.unpack . titleToDash . Text.pack . show) unsup)

requirements' :: Parser [Requirement]
requirements' = taggedList "requirements" requirement

constants' :: Parser [Typed Name]
constants' = taggedListItem "constants" $ typedList identifier

predicates' :: Parser [Predicate]
predicates' = taggedList "predicates" predicate

predicate :: Parser Predicate
predicate = parens $ Predicate <$> identifier <*> typedList variable

variable :: Parser Variable
variable = do
  void $ char '?'
  Variable <$> identifier

functions' :: Parser [Function Variable]
functions' =
  taggedListItem "functions" $ mapM (checkType "number") =<< typedList
    (function variable)

checkType :: Name -> Typed a -> Parser a
checkType expected = \case
  Typed x t | t == [expected] -> pure x
  Typed _ t ->
    fail $ "Type " ++ show t ++ " does not match expected: " ++ show expected
  Untyped x -> pure x

function :: Parser term -> Parser (Function term)
function t = parens $ Function <$> identifier <*> typedList t

action :: Parser Action
action = parens $ do
  void $ string ":action"
  name       <- identifier
  parameters <- do
    isParams <- option False (try $ string ":parameters" >> return True)
    if isParams then parens $ typedList variable else return []
  void $ string ":precondition"
  preconditions <- emptyOr (gd term)
  void $ string ":effect"
  effects <- emptyOr effects'
  return $ Action {..}

emptyOr :: Parser [a] -> Parser [a]
emptyOr v = (try (string "()") >> return []) <|> v

term :: Parser Term
term = (TVar <$> variable) <|> (TName <$> identifier)

effects' :: Parser [Effect]
effects' =
  try (parens $ void (string "and") >> many effect) <|> ((: []) <$> effect)

numericFormula :: Parser NumericFormula
numericFormula = (NumConst <$> number) <|> (NumVar <$> atomicFormula term)

effect :: Parser Effect
effect =
  try
      (   parens
      $   (do
            void $ string "not"
            Del <$> atomicFormula term
          )
      <|> (do
            void $ string "increase"
            void $ parens $ string "total-cost"
            Cost <$> numericFormula
          )
      )
    <|> (Add <$> atomicFormula term)

requirement :: Parser Requirement
requirement = do
  void $ char ':'
  msum $ map
    (\(s, r) -> try $ string s >> pure r)
    [ (LText.fromStrict $ titleToDash $ Text.pack $ show req, req)
    | req <- [minBound .. maxBound]
    ]

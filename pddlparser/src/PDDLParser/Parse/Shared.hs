{-# OPTIONS_GHC -Wno-missing-fields #-}

module PDDLParser.Parse.Shared
  ( atomicFormula
  , gd
  , identifier
  , number
  , parens
  , parseOrErr
  , string
  , whiteSpace
  , taggedItem
  , taggedList
  , typedList
  )
where

import           Control.Monad
import           Data.Char
import           Data.Functor.Identity          ( Identity )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as Text
import qualified Data.Text                     as SText
import           Text.Parsec             hiding ( string )
import           Text.Parsec.Text.Lazy          ( Parser )
import qualified Text.Parsec.Token             as P

import           PDDLParser.PDDL

pddlTokens :: P.GenTokenParser Text st Identity
pddlTokens = P.makeTokenParser P.LanguageDef
  { commentStart  = ""
  , commentEnd    = ""
  , commentLine   = ";"
  , identStart    = letter
  , identLetter   = alphaNum <|> char '-' <|> char '_'
  , reservedNames = ["and", "not", "increase"]
  , caseSensitive = False
  }

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace pddlTokens

lexeme, parens :: Parser a -> Parser a
lexeme = P.lexeme pddlTokens
parens = lexeme . P.parens pddlTokens

identifier :: Parser Name
identifier = lexeme $ Name . SText.pack <$> P.identifier pddlTokens

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

string :: Text -> Parser Text
string = lexeme . fmap Text.pack . mapM caseInsensitiveChar . Text.unpack

parseOrErr :: Parsec Text () a -> String -> Text -> a
parseOrErr p filename = either (error . show) id . parse p filename

taggedList :: Text -> Parser a -> Parser [a]
taggedList name = taggedItem name . many

taggedItem :: Text -> Parser a -> Parser a
taggedItem name item = parens $ do
  void $ string (":" <> name)
  item

typedList :: forall a . Parser a -> Parser [Typed a]
typedList v = go []
 where
  go :: [a] -> Parser [Typed a]
  go currentTerms
    = (do
        void $ string "-"
        ts <-
          parens (string "either" >> many identifier)
            <|> ((: []) <$> identifier)
        (map (`Typed` ts) currentTerms ++) <$> go []
      )
      <|> (optionMaybe v >>= \case
            Nothing -> return $ map Untyped currentTerms
            Just x  -> go (currentTerms ++ [x])
          )

gd :: Parser t -> Parser [Formula t]
gd t =
  try
      (parens $ do
        void $ string "and"
        concat <$> many (gd t)
      )
    <|> ((: []) <$> atomicFormula t)

atomicFormula :: Parser t -> Parser (Formula t)
atomicFormula t = parens $ Formula <$> identifier <*> many t

number :: Parser Number
number = lexeme $ do
  beforeDec <- Text.pack <$> many1 digit
  afterDec  <- optionMaybe $ do
    dot  <- string "."
    digs <- Text.pack <$> many digit
    return $ dot <> digs
  return $ Number $ Text.toStrict $ maybe beforeDec (beforeDec <>) afterDec

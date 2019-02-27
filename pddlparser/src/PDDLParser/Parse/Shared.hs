{-# OPTIONS_GHC -Wno-missing-fields #-}

module PDDLParser.Parse.Shared
  ( atomicFormula
  , gd
  , identString
  , identifier
  , number
  , parens
  , parseOrErr
  , string
  , whiteSpace
  , taggedItem
  , taggedList
  , taggedListItem
  , tryIdentString
  , trystring
  , typedList
  )
where

import           Control.Monad
import           Data.Char
import           Data.Functor.Identity          ( Identity )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as Text
import qualified Data.Text                     as SText
import           Text.Parsec             hiding ( string )
import           Text.Parsec.Text.Lazy          ( Parser )
import qualified Text.Parsec.Token             as P

import           PDDLParser.PDDL

pddlIdentChar :: Parsec Text st Char
pddlIdentChar = alphaNum <|> char '-' <|> char '_'

pddlTokens :: P.GenTokenParser Text st Identity
pddlTokens = P.makeTokenParser P.LanguageDef
  { commentStart  = ""
  , commentEnd    = ""
  , commentLine   = ";"
  , identStart    = letter
  , identLetter   = pddlIdentChar
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

trystring :: Text -> Parser Text
trystring = try . string

identString :: Text -> Parser Text
identString x = lexeme $  do
  res <- fmap Text.pack $ mapM caseInsensitiveChar $ Text.unpack x
  notFollowedBy pddlIdentChar
  return res

tryIdentString :: Text -> Parser Text
tryIdentString = try . identString

parseOrErr :: Parsec Text () a -> String -> Text -> a
parseOrErr p filename = either (error . show) id . parse p filename

taggedList :: Text -> Parser a -> Parser [a]
taggedList name = taggedListItem name . many

taggedListItem :: Text -> Parser [a] -> Parser [a]
taggedListItem name = fmap (fromMaybe []) . taggedItem name

taggedItem :: Text -> Parser a -> Parser (Maybe a)
taggedItem name item = do
  v <- option False $ try $ do
    void $ string "("
    void $ string (":" <> name)
    return True
  if v
    then do
      res <- item
      void $ string ")"
      return $ Just res
    else return Nothing

typedList :: forall a . Parser a -> Parser [Typed a]
typedList v = go []
 where
  go :: [a] -> Parser [Typed a]
  go currentTerms
    = (do
        void $ string "-"
        ts <-
          parens (identString "either" >> many identifier)
            <|> ((: []) <$> identifier)
        (map (`Typed` ts) currentTerms ++) <$> go []
      )
      <|> (optionMaybe v >>= \case
            Nothing -> return $ map Untyped currentTerms
            Just x  -> go (currentTerms ++ [x])
          )

gd :: Parser t -> Parser [Formula t]
gd t =
      (do
        void $ tryIdentString "and"
        concat <$> many (parens $ gd t)
      )
    <|> ((: []) <$> atomicFormula t)

atomicFormula :: Parser t -> Parser (Formula t)
atomicFormula t = Formula <$> identifier <*> many t

number :: Parser Number
number = lexeme $ do
  beforeDec <- Text.pack <$> many1 digit
  afterDec  <- optionMaybe $ do
    dot  <- string "."
    digs <- Text.pack <$> many digit
    return $ dot <> digs
  return $ Number $ Text.toStrict $ maybe beforeDec (beforeDec <>) afterDec

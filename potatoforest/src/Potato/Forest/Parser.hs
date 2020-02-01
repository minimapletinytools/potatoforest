{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}

module Potato.Forest.Parser (

) where

import qualified Potato.Forest.Types        as P
import           Relude

import           Control.Monad              hiding (fail)

import           Data.Char
import           Data.Coerce
import           Data.Default
import qualified Data.Foldable              as Fold
import qualified Data.Set                   as S
import           Data.Text                  (pack, unpack)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data ParserState = ParserState {
  knownItems     :: S.Set  P.ItemId
  , knownRecipes :: S.Set P.RecipeId
}

emptyParserState = ParserState S.empty S.empty

type Parser = StateT ParserState (Parsec Void Text)


runForestParser :: Parser a -> Text -> Either (ParseErrorBundle Text Void) (a, ParserState)
runForestParser p = runParser (runStateT p emptyParserState) "Potato Forest"


sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

number :: Parser Int
number = lexeme L.decimal


reservedWords :: S.Set Text
reservedWords = S.fromList [
  "ITEM"
  , "TITLE"
  , "DESC"
  , "LIMIT"
  , "INPUTS"
  , "OUTPUTS"
  , "RESERVED"
  , "QUANTITY"
  , "TIER"
  , "FULL"
  , "ICON"
  , "RECIPE"
  , "STARTING"
  ]

reservedIdentifiers :: S.Set Text
reservedIdentifiers = S.fromList ["exclusive"] `S.union` reservedWords

reservedWord :: Parser Text
reservedWord = choice . map (try . symbol) $ S.toList reservedWords

data BaseItemExp = BaseItemExp Int  P.ItemId
data RequiredItemExp = ItemExp BaseItemExp | ExclusiveItemExp BaseItemExp

parseBaseItemExp :: Parser BaseItemExp
parseBaseItemExp = do
  amount <- try number <|> return 1
  item <- parseItemId False
  return $ BaseItemExp amount item

parseRequiredItemExp :: Parser RequiredItemExp
parseRequiredItemExp = do
  exclusive <- try (symbol "exclusive" >> return True) <|> return False
  itemExp <- parseBaseItemExp
  if exclusive
    then return (ExclusiveItemExp itemExp)
    else return (ItemExp itemExp)


identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    -- TODO make sure it's not all numbers
    p       = takeWhileP (Just "itemId") isAlphaNum
    check x = if x `S.member` reservedIdentifiers || all isDigit (unpack x)
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

parseItemId :: Bool -> Parser  P.ItemId
parseItemId storeUnique = do
  ps <- get
  let
    knownItems' = knownItems ps
  itemId <-  P.ItemId <$> (lexeme identifier <?> "valid itemId")
  when (coerce itemId `S.member` knownRecipes ps) $ fail "itemId is the same as an existing recipeId"
  when storeUnique $ do
    if itemId `S.member` knownItems'
      then fail "itemId already exists"
      else put $ ps { knownItems = S.insert itemId knownItems' }
  return itemId


-- | helper data struct for parsing items
data OptionalItemFields = OptionalItemFields {
  title      :: Text
  , desc     :: Text
  , limit    :: Maybe Int
  , tier     :: Maybe Int
  , requires :: Maybe [RequiredItemExp]
  , inputs   :: Maybe [BaseItemExp]
  , quantity :: Maybe Int
}

-- can we just do deriving Default?
instance Default OptionalItemFields where
  def = OptionalItemFields {
      title = ""
      , desc = ""
      , limit = Nothing
      , tier = Nothing
      , requires = Nothing
      , inputs = Nothing
      , quantity = Nothing
    }

-- | incrementally adds optional fields for an item
parseOptionalItemFields :: OptionalItemFields -> Parser OptionalItemFields
parseOptionalItemFields oif =
  helper "TITLE" (manyTill asciiChar reservedWord) (\x oif -> oif { title = pack x})
  <|> helper "DESC" (manyTill asciiChar reservedWord) (\x oif -> oif { desc = pack x})
  <|> helper "LIMIT" number (\x oif -> oif { limit = Just x})
  <|> helper "TIER" number (\x oif -> oif { tier = Just x})
  <|> helper "QUANTITY" number (\x oif -> oif { quantity = Just x})
  <|> return oif where
    helper :: Text -> Parser a -> (a -> OptionalItemFields -> OptionalItemFields) -> Parser OptionalItemFields
    helper s p f = try $ do
      symbol s
      x <- p
      parseOptionalItemFields $ f x oif


-- TODO also return Maybe Recipe
parseItem :: Parser P.Item
parseItem = do
  string "ITEM"
  itemId' <- parseItemId True
  oif <- parseOptionalItemFields def
  return $ P.Item {
      itemId = itemId'
      , title = (title :: OptionalItemFields -> Text) oif
      , desc = desc oif
      , limit = limit oif
      , tier = tier oif
    }

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}

module Potato.Forest.Parser (
  forestBlocksParser
  , runForestParser
  , runForestParser'

  -- exported for testing

) where

import qualified Potato.Forest.Types        as P
import           Relude

import           Control.Monad              hiding (fail)

import           Data.Char
import           Data.Coerce
import           Data.Default
import qualified Data.Foldable              as Fold
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Data.Text                  (cons, pack, unpack)
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

-- | same as 'runForestParser' except ignores state
runForestParser' :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
runForestParser' p s = case runForestParser p s of
  Left x  -> Left x
  Right x -> Right $ fst x

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
  , "REQUIRES"
  , "OUTPUTS"
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

reservedOrEof :: Parser ()
reservedOrEof = lookAhead . try $ (space :: Parser ()) *> (void reservedWord <|> eof)

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
    p       = cons <$> letterChar <*> takeWhileP (Just "itemId") (\s -> isAlphaNum s || s == '_')
    check x = if x `S.member` reservedIdentifiers
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
  helper "TITLE" tillReserved (\x oif -> oif { title = pack x})
  <|> helper "DESC" tillReserved (\x oif -> oif { desc = pack x})
  <|> helper "LIMIT" number (\x oif -> oif { limit = Just x})
  <|> helper "TIER" number (\x oif -> oif { tier = Just x})
  <|> helper "QUANTITY" number (\x oif -> oif { quantity = Just x})
  <|> return oif where
    tillReserved = lexeme $ manyTill asciiChar $ reservedOrEof
    helper :: (Show a) => Text -> Parser a -> (a -> OptionalItemFields -> OptionalItemFields) -> Parser OptionalItemFields
    helper s p f = lexeme . try $ do
      symbol s
      x <- p
      parseOptionalItemFields $ f x oif


-- TODO also return Maybe Recipe
parseItem :: Parser P.Item
parseItem = do
  symbol "ITEM"
  itemId' <- parseItemId True
  oif <- parseOptionalItemFields def
  return $ P.Item {
      itemId = itemId'
      , title = (title :: OptionalItemFields -> Text) oif
      , desc = desc oif
      , limit = limit oif
      , tier = tier oif
    }

parseRecipe :: Parser P.Recipe
parseRecipe = fail "not implemented"

parseStarting :: Parser (M.Map P.ItemId Int)
parseStarting = do
  symbol "STARTING"
  itemExprs <- manyTill parseBaseItemExp reservedOrEof
  return $ M.fromList $ map (\(BaseItemExp n i) -> (i, n)) itemExprs

-- | final output type of this parser
data ForestBlocks = ForestBlocks {
  items           :: P.ItemSet
  , recipes       :: P.RecipeSet
  , startingItems :: M.Map P.ItemId Int
} deriving (Show)

-- can I use generics and deriving here?
instance Default ForestBlocks where
  def = ForestBlocks {
      items = S.empty
      , recipes = S.empty
      , startingItems = M.empty
    }

forestBlocksParser_ :: ForestBlocks -> Parser ForestBlocks
forestBlocksParser_ fb =
  helper parseItem (\x fb' -> fb' { items = S.insert x (items fb') } )
  <|> helper parseRecipe (\x fb' -> fb' { recipes = S.insert x (recipes fb')} )
  <|> helper parseStarting (\x fb' -> fb' { startingItems = x } )
  <|> (eof *> return fb) where
    helper :: Parser a -> (a -> ForestBlocks -> ForestBlocks) -> Parser ForestBlocks
    helper p f = try $ do
      x <- p
      forestBlocksParser_ (f x fb)

-- | parser for everything
forestBlocksParser :: Parser ForestBlocks
forestBlocksParser = do
  r <- forestBlocksParser_ def
  return r

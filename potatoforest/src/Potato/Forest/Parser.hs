{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}

module Potato.Forest.Parser (
  ForestBlocks(..)
  , runForestParser
  , runForestParser'
  , runForestBlocksParser

  -- exported for testing
  , identifier
  , parseTextBlob
  , parseTags
  , parseFilename
  , parsePhantom

) where

import qualified Potato.Forest.Types        as P
import           Relude hiding (some, phantom)

import           Data.Char
import           Data.Default
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import        qualified   Data.Text     as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


data ForestBlocks = ForestBlocks {
  knownItems      :: P.ItemSet
  , knownRecipes  :: P.RecipeSet
  , startingItems :: P.Inventory
} deriving (Show)

instance Default ForestBlocks where
  def = ForestBlocks {
      knownItems = S.singleton P.builtin_time
      , knownRecipes = S.empty
      , startingItems = P.Inventory M.empty
    }

itemDNE :: P.ItemId -> Parser a
itemDNE itemId = fail ("item " ++ show itemId ++ " does not exist")

-- | prefix for automatically generate items and recipes
-- thus no identifier is allowed to begin with this
autoPrefix :: Text
autoPrefix = "_____"

-- | adds an item to the ForestBlocks
-- returns True if the item already existed
addItem :: P.Item -> Parser Bool
addItem item = do
  ps <- get
  let
    itemId = P.itemId item
    knownItems' = knownItems ps
    r = isJust (P.lookupItem itemId knownItems')
  put $ ps { knownItems = S.insert item knownItems' }
  return r


-- | adds a starting item to ForestBlocks
-- note if the item exists already (which it shouldn't) it will overwrite it with no warning
addStartingItem :: P.Item -> Int -> Parser ()
addStartingItem item q = do
  ps <- get
  put $ ps { startingItems = P.Inventory $ M.insert item q (P.unInventory . startingItems $ ps) }

-- | adds an recipe to the ForestBlocks
-- returns True if the recipe already existed
addRecipe :: P.Recipe -> Parser Bool
addRecipe recipe = do
  ps <- get
  let
    recipeId = P.recipeId recipe
    knownRecipes' = knownRecipes ps
    r = isJust (P.lookupRecipe recipeId knownRecipes')
  put $ ps { knownRecipes = S.insert recipe knownRecipes' }
  return r


getItem :: P.ItemId -> Parser (Maybe P.Item)
getItem itemId = do
  ps <- get
  return $ P.lookupItem itemId (knownItems ps)



type Parser = StateT ForestBlocks (Parsec Void Text)

-- | runs forest parses with given initial state
runForestParser_ :: ForestBlocks -> Parser a -> Text -> Either (ParseErrorBundle Text Void) (a, ForestBlocks)
runForestParser_ ps p = runParser (runStateT p ps) "Potato Forest"

-- | runs forest parses with empty state
runForestParser :: Parser a -> Text -> Either (ParseErrorBundle Text Void) (a, ForestBlocks)
runForestParser = runForestParser_ def

-- | same as above except discards state (useful for testing)
runForestParser' :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
runForestParser' p t = case runForestParser p t of
  Left e -> Left e
  Right x -> Right $ fst x


sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

-- myLexeme p = sc *> p

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

-- does not lexeme!
reservedWord :: Parser Text
reservedWord = choice . map (try . string) $ S.toList reservedWords

-- | TODO this won't let you use reserved words in descriptions and title :(
lookAheadCommand :: Parser ()
lookAheadCommand = lookAhead . try $ (space :: Parser ()) *> (void reservedWord <|> eof)

lookAheadNewlineCommand :: Parser ()
lookAheadNewlineCommand = lookAhead . try $ eol *> (space :: Parser ()) *> (void reservedWord <|> eof)

identifier_ :: Parser Text
identifier_ = p >>= check where
  p = T.cons <$> letterChar <*> takeWhileP (Just "identifier") (\s -> isAlphaNum s || s == '_')
  check :: Text -> Parser Text
  check x = if all isUpper (T.unpack x) || autoPrefix `T.isPrefixOf` x
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x

identifier :: Parser Text
identifier = lexeme identifier_

parseTextBlob :: Parser Text
parseTextBlob = lexeme $ do
  r <- manyTill asciiChar lookAheadNewlineCommand
  return $ T.pack r

-- | parse a list of space separated tags
parseTags :: Parser [Text]
parseTags = lexeme $ sepBy identifier_ (takeWhile1P Nothing (\x -> x == ' ' || x == '\t'))

-- | parse a filename
parseFilename :: Parser Text
parseFilename = lexeme $ takeWhile1P Nothing (not . isSpace)

parsePhantom :: Parser P.Phantom
parsePhantom = try (symbol "omit" >> return P.Omit)
  <|> try (symbol "pass" >> return P.Pass)
  <?> "omit or pass"

parseItemId :: Parser P.ItemId
parseItemId = P.ItemId <$> (identifier <?> "valid itemId")


class ItemExp a where
  toItemAmount :: a -> (P.ItemId, Int)

data BaseItemExp = BaseItemExp Int  P.ItemId deriving (Show)
data RequiredItemExp = SharedItemExp BaseItemExp | ExclusiveItemExp BaseItemExp deriving (Show)
isRequired :: RequiredItemExp -> Bool
isRequired (SharedItemExp _) = False
isRequired _                 = True

instance ItemExp BaseItemExp where
  toItemAmount (BaseItemExp q i) = (i,q)

instance ItemExp RequiredItemExp where
  toItemAmount (SharedItemExp i)    = toItemAmount i
  toItemAmount (ExclusiveItemExp i) = toItemAmount i

-- |
parseBaseItemExp :: Parser BaseItemExp
parseBaseItemExp = lexeme $ do
  amount <- try number <|> return 1
  item <- parseItemId
  return $ BaseItemExp amount item

parseBaseItemExprList :: Parser [BaseItemExp]
parseBaseItemExprList = manyTill (try parseBaseItemExp) lookAheadCommand

parseRequiredItemExp :: Parser RequiredItemExp
parseRequiredItemExp = lexeme $ do
  exclusive <- try (symbol "exclusive" >> return True) <|> return False
  itemExp <- parseBaseItemExp
  if exclusive
    then return (ExclusiveItemExp itemExp)
    else return (SharedItemExp itemExp)

parseRequiredItemExprList :: Parser [RequiredItemExp]
parseRequiredItemExprList = manyTill parseRequiredItemExp lookAheadCommand


-- | helper data struct for parsing items
data OptionalItemFields = OptionalItemFields {
  title      :: Text
  , desc     :: Text
  , limit    :: Maybe Int
  , tier     :: Maybe Int
  , tags     :: [Text]
  , icon     :: Maybe Text
  , image     :: Maybe Text
  , phantom  :: P.Phantom
  , requires :: Maybe [RequiredItemExp]
  , inputs   :: Maybe [BaseItemExp]
  , quantity :: Maybe Int
  , starting :: Maybe Int
}

-- can we just do deriving Default?
instance Default OptionalItemFields where
  def = OptionalItemFields {
      title = ""
      , desc = ""
      , limit = Nothing
      , tier = Nothing
      , tags = []
      , icon = Nothing
      , image = Nothing
      , phantom = P.Normal
      , requires = Nothing
      , inputs = Nothing
      , quantity = Nothing
      , starting = Nothing
    }

-- | incrementally adds optional fields for an item
parseOptionalItemFields_ :: OptionalItemFields -> Parser OptionalItemFields
parseOptionalItemFields_ oif =
  helper "TITLE" parseTextBlob (\x oif' -> oif' { title = x})
  <|> helper "DESC" parseTextBlob (\x oif' -> oif' { desc = x})
  <|> helper "LIMIT" number (\x oif' -> oif' { limit = Just x})
  <|> helper "TIER" number (\x oif' -> oif' { tier = Just x})
  <|> helper "TAG" parseTags (\x oif' -> oif' { tags = x})
  <|> helper "ICON" parseFilename (\x oif' -> oif' { icon = Just x})
  <|> helper "IMAGE" parseFilename (\x oif' -> oif' { image = Just x})
  <|> helper "REQUIRES" parseRequiredItemExprList (\x oif' -> oif' { requires = Just x})
  <|> helper "PHANTOM" parsePhantom (\x oif' -> oif' { phantom = x})
  <|> helper "INPUTS" parseBaseItemExprList (\x oif' -> oif' { inputs = Just x})
  <|> helper "QUANTITY" number (\x oif' -> oif' { quantity = Just x})
  <|> helper "STARTING" number (\x oif' -> oif' { starting = Just x})
  <|> return oif where
    helper :: Text -> Parser a -> (a -> OptionalItemFields -> OptionalItemFields) -> Parser OptionalItemFields
    helper s p f = lexeme . try $ do
      symbol s
      x <- p
      parseOptionalItemFields_ $ f x oif

parseOptionalItemFields :: Parser OptionalItemFields
parseOptionalItemFields = parseOptionalItemFields_ def

parseItem :: Parser P.Item
parseItem = do
  symbol "ITEM"
  itemId' <- parseItemId
  oif <- parseOptionalItemFields
  let item = P.Item {
      itemId = itemId'
      , title = (title :: OptionalItemFields -> Text) oif
      , desc = desc oif
      , limit = limit oif
      , tier = tier oif
    }
  exists <- addItem item
  when exists $ fail $ "item " ++ show itemId' ++ " already exists"
  return item

buildInventory_ :: (ItemExp a) => [a] -> Parser P.Inventory
buildInventory_ exps = do
  knownItems' <- knownItems <$> get
  let
    i1 = map toItemAmount exps
    mapFn (i,q) = case P.lookupItem i knownItems' of
      Nothing -> itemDNE i
      Just x  -> return (x, q)
  i2 <- mapM mapFn i1
  return . P.Inventory $ M.fromList i2

buildInventory :: (ItemExp a) => Maybe [a] -> Parser P.Inventory
buildInventory mexps = case mexps of
  Nothing   -> return $ P.Inventory M.empty
  Just exps -> buildInventory_ exps

-- | parse rest of item definition
-- returns (quantity, recipe)
parseItemRest :: Parser (Maybe Int, Maybe P.Recipe)
parseItemRest = do
  symbol "ITEM"
  itemId' <- parseItemId
  oif <- parseOptionalItemFields
  existingItem <- getItem itemId'
  case existingItem of
    Nothing -> itemDNE itemId'
    Just item -> do
      let
        rstarting = starting oif
      case rstarting of
        Nothing -> return ()
        Just q -> addStartingItem item q
      rrecipe <- if isNothing (requires oif) && isNothing (inputs oif)
        then return Nothing
        else do
          exclusiveRequires' <- buildInventory (fmap (filter isRequired) (requires oif))
          requires' <- buildInventory (fmap (filter (not .isRequired)) (requires oif))
          inputs' <- buildInventory (inputs oif)
          let
            recipeId' = P.RecipeId $ itemId' & (\(P.ItemId i) ->  autoPrefix <> i)
            recipe = P.Recipe {
                recipeId = recipeId'
                , requires = requires'
                , exclusiveRequires = exclusiveRequires'
                , inputs = inputs'
                , outputs = P.Inventory $ M.singleton item (fromMaybe 1 (quantity oif))
              }
          exists <- addRecipe recipe
          when exists $ fail $ "recipe " ++ show recipeId' ++ " already exists"
          return $ Just recipe
      return (rstarting, rrecipe)

-- | helper data struct for parsing items
data OptionalRecipeFields = OptionalRecipeFields {
  rrequires  :: Maybe [RequiredItemExp]
  , rinputs  :: Maybe [BaseItemExp]
  , routputs :: Maybe [BaseItemExp]
}

-- can we just do deriving Default?
instance Default OptionalRecipeFields where
  def = OptionalRecipeFields {
      rrequires = Nothing
      , rinputs = Nothing
      , routputs = Nothing
    }

-- | incrementally adds optional fields for a Recipe
parseOptionalRecipeFields_ :: OptionalRecipeFields -> Parser OptionalRecipeFields
parseOptionalRecipeFields_ orf =
  helper "REQUIRES" parseRequiredItemExprList (\x orf' -> orf' { rrequires = Just x})
  <|> helper "INPUTS" parseBaseItemExprList (\x orf' -> orf' { rinputs = Just x})
  <|> helper "OUTPUTS" parseBaseItemExprList (\x orf' -> orf' { routputs = Just x})
  <|> return orf where
    helper :: Text -> Parser a -> (a -> OptionalRecipeFields -> OptionalRecipeFields) -> Parser OptionalRecipeFields
    helper s p f = lexeme . try $ do
      symbol s
      x <- p
      parseOptionalRecipeFields_ $ f x orf

parseOptionalRecipeFields :: Parser OptionalRecipeFields
parseOptionalRecipeFields = parseOptionalRecipeFields_ def

parseRecipe :: Parser P.Recipe
parseRecipe = do
  symbol "RECIPE"
  recipeId' <- P.RecipeId <$> (identifier <?> "valid recipeId")
  orf <- parseOptionalRecipeFields
  let
    rrequires' = rrequires orf
    rinputs' = rinputs orf
    routputs' = routputs orf
  when (isNothing rrequires' && isNothing rinputs') $
    fail $ "recipe " ++ show recipeId' ++ " must have requirements or inputs"
  when (isNothing routputs') $
    fail $ "recipe " ++ show recipeId' ++ " must have output"
  exclusiveRequires' <- buildInventory (fmap (filter isRequired) rrequires')
  requires' <- buildInventory (fmap (filter (not .isRequired)) rrequires')
  inputs' <- buildInventory rinputs'
  outputs' <- buildInventory routputs'
  let recipe = P.Recipe {
      recipeId = recipeId'
      , requires = requires'
      , exclusiveRequires = exclusiveRequires'
      , inputs = inputs'
      , outputs = outputs'
    }
  exists <- addRecipe recipe
  when exists $
    fail $ "recipe " ++ show recipeId' ++ " already exists"
  return recipe


-- TODO this does nothing right now, it should store results in state
-- make sure to document in README.md
parseStarting :: Parser (M.Map P.ItemId Int)
parseStarting = do
  symbol "STARTINGITEMS"
  itemExprs <- parseBaseItemExprList
  return $ M.fromList $ map (\(BaseItemExp n i) -> (i, n)) itemExprs


line :: Parser Text
line = lexeme $ takeWhileP (Just "line") (not . (flip elem ("\r\n" :: String)))

-- | parses items only and stores results in ForestBlocks
parseItemsOnly :: Parser ()
parseItemsOnly =
  try (void parseItem *> parseItemsOnly)
  <|> eof
  <|> (line *> parseItemsOnly)
  <?> "ITEM"

-- | parses everything but items and stores results in ForestBlocks
parseRest :: Parser ()
parseRest =
  try (void parseItemRest *> parseRest)
  <|> try (void parseRecipe *> parseRest)
  -- <|> try (void parseStarting *> parseRest)
  <|> eof
  <|> (line *> parseRest)
  <?> "ITEM or RECIPE"

-- | our parser routine for everything
runForestBlocksParser :: Text -> Either (ParseErrorBundle Text Void) ForestBlocks
runForestBlocksParser s = case runForestParser parseItemsOnly s of
  Left e -> Left e
  Right (_, itemsOnlyState) -> case runForestParser_ itemsOnlyState parseRest s of
    Left e                -> Left e
    Right (_, finalState) -> Right finalState


module Potato.Forest.Types (
  ItemId(..)
  , RecipeId(..)
  , Item(..)
  , Recipe(..)
  , lookupItem
  , lookupRecipe
  , Inventory
  , ItemSet
  , RecipeSet
) where


import           Relude

import           Potato.Forest.Internal.Containers


import qualified Data.Map                          as M
import qualified Data.Set                          as S
import qualified Data.Text                         as T

import           Data.Default

newtype ItemId = ItemId T.Text deriving (Eq, Ord, Show)
newtype RecipeId = RecipeId T.Text deriving (Eq, Ord, Show)

-- | internal types
data Item = Item {
  itemId  :: ItemId
  , title :: T.Text
  , desc  :: T.Text
  , limit :: Maybe Int
  , tier  :: Maybe Int
} deriving (Show)

instance Ord Item where
  (<=) a b = itemId a <= itemId b
-- using "deriving Eq" instance rather than using Ord from above so we manually specify. IDK why
instance Eq Item where
  (==) a b = itemId a == itemId b

instance Default Item where
  def = Item {
      itemId = ItemId ""
      , title = ""
      , desc = ""
      , limit = Nothing
      , tier = Nothing
    }

-- | time is the only built in item
builtin_time :: Item
builtin_time = Item {
    itemId = ItemId "time"
    , title = "time"
    , desc = "1 unit of time"
    , limit = Nothing
    , tier = Just 0
  }

-- | map of item to quantity
type Inventory = M.Map Item Int

data Recipe = Recipe {
    recipeId            :: RecipeId
    , requires          :: Inventory
    , exclusiveRequires :: Inventory
    , inputs            :: Inventory
    , outputs           :: Inventory
  } deriving (Show)

instance Ord Recipe where
  (<=) a b = recipeId a <= recipeId b
-- using "deriving Eq" instance rather than using Ord from above. IDK why
instance Eq Recipe where
  (==) a b = recipeId a == recipeId b



instance Default Recipe where
  def = Recipe {
      recipeId = RecipeId ""
      , requires = M.empty
      , exclusiveRequires = M.empty
      , inputs = M.empty
      , outputs = M.empty
    }

type ItemSet = S.Set Item
type RecipeSet = S.Set Recipe

-- | list of required item and set of recipes that use that item
type ItemConnections = M.Map Item RecipeSet
-- | maps an Item to its ItemConnections
type ItemConnectionsMap = M.Map Item ItemConnections

lookupItem :: ItemId -> ItemSet -> Maybe Item
lookupItem i = lookup (def {itemId = i})

lookupRecipe :: RecipeId -> RecipeSet -> Maybe Recipe
lookupRecipe i = lookup (def {recipeId = i})


findItemConnections :: RecipeSet -> Item -> ItemConnections
findItemConnections = undefined

-- | does not handle forced tiers yet
-- note that the first pass of this function should completely populate ItemConnectionsMap (unless we do early exit)
findNextTier ::
  RecipeSet -- ^ all recipes
  -> ItemConnectionsMap -- ^ all connections
  -> ItemSet -- ^ list of items in lower tiers
  -> ItemSet -- ^ all items we want to search (must exclude items in lower tiers)
  -> ItemSet -- ^ returns set of items in next tier
findNextTier recipes allConns lowerTiers searchItems = S.filter ffn searchItems where
  ffn item' = case M.lookup item' allConns of
    -- no ItemConnections means it's a tier 0 item
    Nothing -> null lowerTiers
    -- all required items are contained in lower tiers
    Just x  -> M.foldrWithKey (\k _ acc -> acc && S.member k lowerTiers) True x

generateTieredItems ::
  ItemSet -- ^ all items
  -> RecipeSet -- ^ all recipes
  -> [ItemConnectionsMap] -- ^ list of each set of Items and their ItemConnections for each tier (starting at 0)
generateTieredItems items recipes = unfoldr getNextTieredItems (S.empty, items) where
  allConnections = mapSetToMap (findItemConnections recipes) items
  getNextTieredItems :: (ItemSet, ItemSet) -> Maybe (ItemConnectionsMap, (ItemSet, ItemSet))
  getNextTieredItems (lowerTiers, searchItems) = r where
    r1 = findNextTier recipes allConnections lowerTiers searchItems
    r2 = (S.union lowerTiers r1, searchItems S.\\ r1)
    r1c = mapSetToMap (\item -> M.findWithDefault M.empty item allConnections) r1
    r = if S.null searchItems then Nothing else Just (r1c, r2)

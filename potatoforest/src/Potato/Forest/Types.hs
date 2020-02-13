{-# LANGUAGE DeriveAnyClass #-}

module Potato.Forest.Types (
  ItemId(..)
  , RecipeId(..)
  , Phantom(..)
  , Item(..)
  , Recipe(..)
  , builtin_time
  , lookupItem
  , lookupRecipe
  , Inventory(..)
  , ItemSet
  , RecipeSet
) where


import           Potato.Forest.Internal.Containers
import           Relude
import qualified Text.Show

import qualified Data.Map                          as M
import qualified Data.Set                          as S
import qualified Data.Text                         as T

import           Data.Default

newtype ItemId = ItemId { unItemId :: T.Text } deriving newtype (Eq, Ord, Show, IsString, NFData)
newtype RecipeId = RecipeId { unRecipeId :: T.Text } deriving newtype (Eq, Ord, Show, IsString, NFData)

data Phantom = Normal | Omit | Pass deriving (Eq, Show, Generic, NFData)

-- | internal types
data Item = Item {
  itemId  :: ItemId
  , title :: T.Text
  , desc  :: T.Text
  , limit :: Maybe Int
  , tier  :: Maybe Int
} deriving (Generic, NFData)

-- temporary for testing
instance Show Item where
  show item = show (itemId item)

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
builtin_time = def {
    itemId = ItemId "time"
    , title = "time"
    , desc = "1 unit of time"
  }

-- | map of item to quantity
newtype Inventory = Inventory { unInventory :: M.Map Item Int } deriving newtype (NFData)

instance Show Inventory where
  show (Inventory inv) = show $ M.mapKeysWith const itemId inv

data Recipe = Recipe {
    recipeId            :: RecipeId
    , requires          :: Inventory
    , exclusiveRequires :: Inventory
    , inputs            :: Inventory
    , outputs           :: Inventory
  } deriving (Show, Generic, NFData)

instance Ord Recipe where
  (<=) a b = recipeId a <= recipeId b
-- using "deriving Eq" instance rather than using Ord from above. IDK why
instance Eq Recipe where
  (==) a b = recipeId a == recipeId b

instance Default Recipe where
  def = Recipe {
      recipeId = RecipeId ""
      , requires = Inventory M.empty
      , exclusiveRequires = Inventory M.empty
      , inputs = Inventory M.empty
      , outputs = Inventory M.empty
    }

type ItemSet = S.Set Item
type RecipeSet = S.Set Recipe

lookupItem :: ItemId -> ItemSet -> Maybe Item
lookupItem i = lookup (def {itemId = i})

lookupRecipe :: RecipeId -> RecipeSet -> Maybe Recipe
lookupRecipe i = lookup (def {recipeId = i})

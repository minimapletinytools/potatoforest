
module Potato.Forest.Types (
  ItemId(..)
  , RecipeId(..)
  , Item(..)
  , Recipe(..)
  , builtin_time
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

newtype ItemId = ItemId { unItemId :: T.Text } deriving (Eq, Ord, Show)
newtype RecipeId = RecipeId { unRecipeId :: T.Text } deriving (Eq, Ord, Show)

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

lookupItem :: ItemId -> ItemSet -> Maybe Item
lookupItem i = lookup (def {itemId = i})

lookupRecipe :: RecipeId -> RecipeSet -> Maybe Recipe
lookupRecipe i = lookup (def {recipeId = i})

module Potato.Forest.Types (

) where

import qualified Data.Text as T
import qualified Data.Map as M

-- | parser item
{-
data ParserItem = ParserItem {
  itemId      :: T.Text
  , itemTitle :: T.Text
  , itemDesc  :: T.Text
}
data BaseItemExp = BaseItemExp Int T.Text
data ItemExp = ItemExp BaseItemExp | ExclusiveItemExp BaseItemExp
-}


newtype ItemId = ItemId T.Text
-- | internal types
data Item = Item {
  itemId :: ItemId
  , title :: T.Text
  , desc :: T.Text
  , limit :: Maybe Int
  , tier :: Maybe Int
}

builtin_time :: Item
builtin_time = Item {
  itemId = ItemId "time"
  , title = "time"
  , desc = "1 unit of time"
  , limit = Nothing
  , tier = Just 0
}

-- | map of item to starting quantity
type Inventory = M.Map ItemId Int

data Recipe = Recipe {
  recipeId :: T.Text
  , requires :: Inventory
  , exclusiveRequires :: Inventory
  , inputs :: Inventory
  , outputs :: Inventory
}

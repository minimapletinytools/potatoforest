
module Potato.Forest.Types (

) where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Set.Unicode as S

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
newtype RecipeId = RecipeId T.Text

-- | internal types
data Item = Item {
  itemId :: ItemId
  , title :: T.Text
  , desc :: T.Text
  , limit :: Maybe Int
  , tier :: Maybe Int
}


instance Ord Item where
  (<=) a b = itemId a <= itemId b

instance Eq Item

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
  recipeId :: T.Text
  , requires :: Inventory
  , exclusiveRequires :: Inventory
  , inputs :: Inventory
  , outputs :: Inventory
}

instance Ord Recipe where
  (<=) a b = recipeId a <= recipeId b

instance Eq Recipe

-- logically these are just sets as Items and Recipes are immutable
-- but semantically we want often want to treat them as maps
-- TODO switch to set?
type ItemSet = S.Set Item
type RecipeSet = S.Set Recipe

-- list of required item and set of recipes that use that item
type ItemConnections = M.Map Item RecipeSet
type ItemConnectionsMap = M.Map Item ItemConnections

-- | converts a set to map
mapSetToMap :: (a -> b) -> S.Set a -> M.Map a b
mapSetToMap f S.Tip = M.Tip
mapSetToMap f (S.Bin sz elt ls rs) = M.Bin sz elt (f elt) ls rs

findItemConnections :: RecipeSet -> Item -> ItemConnections
findItemConnections = undefined

-- you can delete me now :)
filterAccumL :: (a -> b -> (a,Bool)) -> a -> [b] -> (a,[b])
filterAccumL f a bs = (out_a, out_bs) where
  (out_a, cs) = mapAccumL f a bs
  out_bs = catMaybes $ zipWith (\a' c' -> if c' then Just a' else Nothing) cs

-- | does not handle forced tiers yet
-- note that the first pass of this function should completely populate ItemConnectionsMap (unless we do early exit)
findNextTier ::
  RecipeSet -- ^ all recipes
  -> ItemConnectionsMap -- ^ all connections
  -> ItemSet -- ^ list of items in lower tiers
  -> ItemSet -- ^ all items we want to search (must exclude items in lower tiers)
  -> ItemSet -- ^ returns set of items in next tier
findNextTier recipes allConns lowerTiers searchItems = filter ffn allConns searchItems where
  ffn allConns' item' = case M.lookup item' allConns of
    -- no ItemConnections means it's a tier 0 item
    Nothing -> if null lowerTiers then True else False
    -- all required items are contained in lower tiers
    Just x -> M.foldrWithKey (\k _ acc -> acc && member k lowerTiers) x

generateTieredItems ::
  ItemSet -- ^ all items
  -> RecipeSet -- ^ all recipes
  -> [ItemConnectionsMap] -- ^ list of each set of Items and their ItemConnections for each tier (starting at 0)
generateTieredItems items recipes = unfoldr getNextTieredItems (S.empty, items) where
  allConnections = mapSetToMap (findItemConnections recipes) items
  getNextTieredItems :: (ItemSet, ItemSet) -> Maybe (ItemConnectionsMap, (ItemSet, ItemSet))
  getNextTieredItems (lowerTiers, searchItems) = r where
    r1 = findNextTier recipes allConnections lowerTiers searchItems
    r2 = (lowerTiers S.∪ r1, searchItems S.∖ r1)
    r = if S.null searchItems then Nothing else (r1, r2)

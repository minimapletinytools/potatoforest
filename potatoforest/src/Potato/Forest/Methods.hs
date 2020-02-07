module Potato.Forest.Methods (
  helloPotato
  , generateTieredItems
  , sortTieredItems

  , ItemConnections
  , ItemConnectionsMap
  , ItemConnectionsList
) where

import           Relude

import           Potato.Forest.Internal.Containers
import           Potato.Forest.Types

import qualified Data.Map                          as M
import qualified Data.Set                          as S

helloPotato :: Text
helloPotato = "hello potato"

-- | list of required item and set of recipes that use that item
type ItemConnections = M.Map Item RecipeSet
-- | maps an Item to its ItemConnections
type ItemConnectionsMap = M.Map Item ItemConnections
-- | sorted list variant (for rendering)
type ItemConnectionsList = [(Item, ItemConnections)]

addRecipeToItemConnections :: ItemConnections -> Recipe -> ItemConnections
addRecipeToItemConnections itemConns recipe = r where
  allInputs = unInventory (requires recipe) `M.union` unInventory (exclusiveRequires recipe) `M.union` unInventory (inputs recipe)
  r = M.foldrWithKey (\k _ acc -> M.insertWith S.union k (S.singleton recipe) acc) itemConns allInputs

findItemConnections :: RecipeSet -> Item -> ItemConnections
findItemConnections recipes item = r where
  recipes' = S.filter (\recipe -> item `M.member` unInventory (outputs recipe)) recipes
  r = S.foldl addRecipeToItemConnections M.empty recipes'

-- | does not handle forced tiers yet
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
    -- find the next tier
    r1' = findNextTier recipes allConnections lowerTiers searchItems

    -- empty means we've found all items, what's left is circular
    -- for now just lump all circular dependencies at the end
    -- TODO actually do this (you'll have to do some graph crawling function that marks visited spots)
    r1 = if S.null r1'
      then searchItems
      else r1'

    -- add found items to lowerTiers and remove it from our search items
    r2 = (S.union lowerTiers r1, searchItems S.\\ r1)
    -- reconnect found items with its ItemConnections
    r1c = mapSetToMap (\item -> M.findWithDefault M.empty item allConnections) r1
    r = if S.null searchItems then Nothing else Just (r1c, r2)


sortTieredItems ::
  [ItemConnectionsMap]
  -> [ItemConnectionsList]
sortTieredItems tiers = map M.toList tiers
  -- map of existing X positions
  -- for each tier (first pass)
    -- set temp x position to be average of x pos of dependencies
  -- sort tier by temp x pos
  -- set actual x position by order (center aligned)

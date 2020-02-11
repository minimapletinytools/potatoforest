module Potato.Forest.Methods2 (
  helloPotato
  , generateTieredItems
  , sortTieredItems

  , ItemConnections
  , ItemConnectionsMap
  , ItemConnectionsList
) where

import           Data.Functor.Classes              (Ord1, compare1)
import           Relude

import           Potato.Forest.Internal.Containers
import           Potato.Forest.Types

import qualified Data.Map                          as M
import qualified Data.Set                          as S

-- | data type containing information to compute a node's tier
-- (parent, child)
type Adjs = (S.Set Item, S.Set Item)


buildAdjs ::
  ItemConnectionsMap -- ^ all connections
  -> M.Map Item Adjs -- ^ accumulated map of adjancies
  -> Item -- ^ item to discover tier for
  -> M.Map Item Adjs -- ^ new map of adjacencies
buildAdjs allCons forced visited item = r where

buildAdjs :: M.Map Item

-- | evaluate the tier of an item
-- returns (tier, new map with tier inserted)
evalTier ::
  M.Map Item Adjs -- ^ map of adjancencies made in the previous step
  -> M.Map Item (Maybe Int) -- ^ accumulating map of discovered/forced tiers OR visited but not yet known tiers
  -> (Item, Adjs) -- ^ current item we're processing
  -> (Maybe Int, M.Map Item (Maybe Int)) -- ^ (our own tier, new accumulating map)
evalTier adjs discovered current = r where
  r = undefined

-- | data type containing information to compute a node's tier
-- (parent tiers, child tiers)
type TierFn = ([Maybe Int], [Maybe Int])

-- | convert a fully constructed TierFn to a node's actual tier
evalTierFn :: TierFn -> Maybe Int
evalTierFn (ps, cs) = min_minps_maxcsp1 (M.foldr minps Nothing ps) (M.foldr maxcs Nothing cs) where
  -- compute the min of two parent tiers
  minps :: Maybe Int -> Maybe Int -> Maybe Int
  minps Nothing p2 = p2
  minps p1 Nothing = p1
  minps p1 p2      = minOrd1 p1 p2
  -- compute the max of two child tiers
  maxcsp1 :: Maybe Int -> Maybe Int -> Maybe Int
  maxcsp1 a b = (+1) <$> maxOrd1 a b
  -- compute the new tier from min/max of parent/child tiers
  min_minps_maxcsp1 :: Maybe Int -> Maybe Int -> Int
  -- no child tiers means we are tier 0
  min_minps_maxcsp1 _ Nothing     = Just 0
  -- no parent tiers means we use the child tier
  min_minps_maxcsp1 Nothing maxcs = maxcsp1
  -- otherwise just regular min
  min_minps_maxcsp1 minps maxcs   = minOrd1 minps maxcsp1

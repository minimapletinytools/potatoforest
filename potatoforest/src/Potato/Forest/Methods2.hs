module Potato.Forest.Methods2 (
  findTiers
  , newGenerateTieredItems

  -- exported for testing
  buildAdjs
  , clearInChildren
  , clearInParents
) where

import           Data.Functor.Classes              (Ord1, compare1)
import           Relude
import           Relude.Extra.Lens

import           Potato.Forest.Internal.Containers
import           Potato.Forest.Methods
import           Potato.Forest.Types

import qualified Data.Map                          as M
import           Data.Maybe
import qualified Data.Set                          as S

-- Notation/Termionology:
-- A -> B means "A depends on B"
-- and "B is the child of A"
-- and "A is the parent of B"

maxOrd1 :: (Ord1 f, Ord a)  => f a -> f a -> f a
maxOrd1 a b = case compare1 a b of
  GT -> a
  _  -> b

minOrd1 :: (Ord1 f, Ord a)  => f a -> f a -> f a
minOrd1 a b = case compare1 a b of
  LT -> a
  _  -> b

_1 :: Lens' (a,b) a
_1 f (a, b) = fmap (\a' -> (a', b)) (f a)

_2 :: Lens' (a,b) b
_2 f (a, b) = fmap (\b' -> (a, b')) (f b)

-- | data type containing information to compute a node's tier
-- (parent, child)
type Adjs = ([Item], [Item])

-- |
buildAdjs ::
  ItemConnectionsMap -- ^ all connections
  -> Item -- ^ starting item to compute adjacencies for
  -> M.Map Item Adjs -- ^ accumulated map of adjancies
  -> M.Map Item Adjs -- ^ new map of adjacencies
buildAdjs allCons item visited = if item `M.member` visited
  -- if we've been here before, then there's nothing to do
  then visited
  else r where
    -- list of our children
    children :: [Item]
    children = S.toList . M.keysSet $ M.findWithDefault M.empty item allCons
    -- add ourselves to visited nodes
    r0 = M.insert item ([], children) visited
    -- recursively compute Adjs for our children
    foldFn1 :: Item -> Map Item Adjs -> Map Item Adjs
    foldFn1 child acc = buildAdjs allCons child acc
    r1 = foldr foldFn1 r0 children
    -- finally add ourselves to each child as a parent
    foldFn2 :: Item -> Map Item Adjs -> Map Item Adjs
    foldFn2 child acc = M.alter (Just . over _1 ([item]<>) . fromMaybe ([],[])) child acc
    r :: M.Map Item Adjs
    r = foldr foldFn2 r1 children

-- | evaluate the tier of an item
-- returns (tier, new map with tier inserted)
evalTier ::
  M.Map Item Int -- ^ forced tiers
  -> M.Map Item Adjs -- ^ map of adjancencies made in the previous step
  -> Either Int Int -- ^ left is parent of this node's tier, right is child of this node's tier
  -> M.Map Item (Maybe Int) -- ^ accumulating map of node tiers (or Nothing if still being computed)
  -> Item -- ^ current item we're processing
  -- TODO pretty sure Maybe not necessary in left case
  -> (M.Map Item (Maybe Int), Either (Maybe Int) (Maybe Int)) -- ^ (new accumulating map, our own tier (left if just parent or child, right if eventual parent and child, Nothing if no fixed pt))
evalTier forced adjs pt disc item = if item `M.member` disc
  -- we've been here before, `Right _` indicates no fixed pt and both parent and child
  then (disc, Right (M.lookup item forced))
  else r where
    -- if not found, it must be an isolated node
    (ps, cs) = M.findWithDefault ([],[]) item adjs
    -- put self as Nothing in disc to indicate we are computing it
    r0 = M.insert item Nothing disc

    -- recursively call on both parent and children
    (r1, psts') = mapAccumR (evalTier forced adjs (Left tier)) r0 ps
    (r2, csts') = mapAccumR (evalTier forced adjs (Right tier)) r1 cs

    -- search children first
      -- if child returns a Right process as normal
    -- search parents
      -- if parent returns a Right, just ignore (already processed this case, either we just came from this parent OR we looped to it from a child node)

    -- if all ri

    -- TODO also this needs to propogate the Right cases back to calling parents meaning this is totally broken :((
    -- TODO must be assymmetry here
    -- loop detection should only happen in one direction to both! I think at least
    -- combine results for adjacent nodes that are eventual parents and children
    combine :: [Either (Maybe Int) (Maybe Int)] -> [Either (Maybe Int) (Maybe Int)] -> [Maybe Int]
    combine orig add = map (either id id) orig <> rights add
    psts = combine psts' csts'
    csts = combine csts' psts'

    -- compute tier
    tier' = evalTierFn (psts, csts)
    -- and override with forced tier
    tier = M.findWithDefault tier' item forced

    -- left means we have a fixed point (probably)
    r = (r2, Left (Just tier))

clearInTuple ::
  Lens' ([Item],[Item]) [Item]
  -> Item -- ^ item to clear
  -> M.Map Item Adjs -- ^ map of adjacencies
  -> M.Map Item Adjs -- ^ map with item removed from parents of all of item's children
clearInTuple l item adjs = M.adjust (over l (delete item)) item adjs

clearInChildren :: Item -> M.Map Item Adjs -> M.Map Item Adjs
clearInChildren = clearInTuple _2

clearInParents :: Item -> M.Map Item Adjs -> M.Map Item Adjs
clearInParents = clearInTuple _1


-- | evaluate the tier of an item
-- returns (tier, new map with tier inserted)
-- fixed version of above
evalTier2 ::
  M.Map Item Int -- ^ forced tiers
  -> M.Map Item Adjs -- ^ map of adjancencies made in the previous step
  -> Maybe (Either Int Int) -- ^ left is parent of this item's tier, right is child of this item's tier
  -> M.Map Item (Maybe Int) -- ^ accumulating map of node tiers (or Nothing if still being computed)
  -> Item -- ^ current item we're processing
  -> (M.Map Item (Maybe Int), Either Int (Maybe Int)) -- ^ (new accumulating map, our own tier (left if just parent or child, right if eventual parent and child, Nothing if already visited and no forced value)
evalTier2 forced adjs mct disc item = if item `M.member` disc
  -- we've been here before, `Right _` indicates no fixed pt and both parent and child
  then (disc, Right (M.lookup item forced))
  else r where
    -- (note, if not found, it must be an isolated node)
    (ps, cs) = M.findWithDefault ([],[]) item adjs
    -- put self as Nothing in disc to indicate we are computing it
    r0 = M.insert item Nothing disc
    -- remove self from child's parents before recursing
    adjs1 = clearInChildren item adjs
    -- recursively call in all children, pass in a our own tier as a thunk
    (r1, csts'') = mapAccumR (evalTier forced adjs (Just $ Left tier)) r0 cs
    -- remove self from parent's children (yes adjs, not adjs1, though both would work)
    adjs2 = clearInParents item adjs
    -- recursively call in all parents, pass in a our own tier as a thunk
    (r2, psts'') = mapAccumR (evalTier forced adjs (Just $ Right tier)) r1 ps
    -- add caller's tier to parent or children
    (psts', csts') = case mct of
      Left x -> (Left x:psts'',csts'')
      Right x -> (psts'', Left x:csts'')
    -- convert to tierfn
    csts = map (either Just id) csts'
    -- if parent returns Right, do nothing because we've included that data in other ways
    -- also add children that are also eventual parents of this item
    -- note that this is the same as parents that are eventual children of this item
    -- so we don't need to add parents that are eventual children of this item to children (nor do we even know if this is the case or not)
    psts = rights csts' <> map Just (lefts psts')
    -- create a thunk for our tier
    tier = evalTierFn (psts, csts)
    -- return our accumulated visited nodes and our tier
    r = (r2, Left tier)

-- | data type containing information to compute a node's tier
-- (parent tiers, child tiers)
type TierFn = ([Maybe Int], [Maybe Int])

-- | convert a fully constructed TierFn to a node's actual tier
evalTierFn :: TierFn -> Int
evalTierFn (ps, cs) = r where
  -- compute the min of two parent tiers
  minps :: Maybe Int -> Maybe Int -> Maybe Int
  minps Nothing p2 = p2
  minps p1 Nothing = p1
  minps p1 p2      = minOrd1 p1 p2
  -- compute the max of two child tiers
  maxcs :: Maybe Int -> Maybe Int -> Maybe Int
  maxcs = maxOrd1
  -- compute the new tier from min/max of parent/child tiers
  combine_minps_maxcs :: Maybe Int -> Maybe Int -> Int
  -- No fixed pt in both parent and child, this means we hit a loop, so our tier is 0
  combine_minps_maxcs Nothing Nothing       = 0
  -- Nothing in child tier means use the parent tier
  combine_minps_maxcs (Just mps) Nothing    = mps - 1
  -- no parent tiers means we use the child tier plus 1
  combine_minps_maxcs Nothing (Just mcs)    = mcs + 1
  -- otherwise take the min
  combine_minps_maxcs (Just mps) (Just mcs) = min (mps - 1) (mcs + 1)
  r = if null cs
    -- if there are NO child nodes, we have no dependencies so our tier is 0
    then 0
    else combine_minps_maxcs (S.foldr minps Nothing ps) (S.foldr maxcs Nothing cs)

-- TODO TEST
-- | returns the tier of the item according to rules (see README.md)
findTiers ::
  ItemConnectionsMap -- ^ all connections
  -> M.Map Item Int -- ^ known or forced tiers
  -> Item -- ^ starting item
  -> M.Map Item Int -- ^ all item tiers
findTiers allConns forced item = tiers where
  -- first build adjacency map
  adjs = buildAdjs allConns item M.empty
  -- next evaluate tiers
  (mtiers, _) = evalTier forced adjs M.empty item
  -- all tiers should be Just otherwise it's a bug
  !tiers = fmap fromJust mtiers


newGenerateTieredItems ::
  ItemSet -- ^ all items
  -> RecipeSet -- ^ all recipes
  -> M.Map Int ItemSet -- ^ map of tiers to item in each tier
newGenerateTieredItems items recipes = tierToItem where
  allConns = mapSetToMap (findItemConnections recipes) items

  -- | keep finding tiers of items until we've found all
  untilEmpty :: ItemSet -> M.Map Item Int -> (ItemSet, M.Map Item Int)
  untilEmpty rest known = if S.null rest then (S.empty, known) else r where
    -- take the first item, (must exist since set is non empty)
    item = S.findMin rest
    newKnown = findTiers allConns M.empty item
    newRest = rest S.\\ M.keysSet newKnown
    -- Nothing means circular dependency with no other dependencies which is tier 0
    r = untilEmpty newRest newKnown

  (_, itemToTier) = untilEmpty items M.empty

  -- reverse keys/values
  foldfn :: Item -> Int -> M.Map Int ItemSet -> M.Map Int ItemSet
  foldfn item t acc =  M.insert t (item `S.insert` M.findWithDefault S.empty t acc) acc
  tierToItem = M.foldrWithKey foldfn M.empty itemToTier

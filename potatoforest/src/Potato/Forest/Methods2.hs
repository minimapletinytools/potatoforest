{-# LANGUAGE PatternGuards #-}
module Potato.Forest.Methods2 (
  findTiers
  , newGenerateTieredItems

  -- exported for testing
  , evalTierFn'
  , buildAdjs
  , clearItemFromChildren
  , clearItemFromParents
) where

import           Data.Functor.Classes              (Ord1, compare1)
import           Relude
import           Relude.Extra.Lens

import           Potato.Forest.Internal.Containers
import           Potato.Forest.Methods
import           Potato.Forest.Types

import qualified Data.List                         as L
import qualified Data.Map                          as M
import           Data.Maybe
import qualified Data.Set                          as S

-- for debugging, remove me
import qualified Data.Text.Lazy                    as LT
import           Text.Pretty.Simple
trace_pShow :: (Show a) => a -> b -> b
trace_pShow x = trace (LT.unpack (pShow x))

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
-- (parents, children)
type Adjs = ([Item], [Item])

-- | fold method to convert ItemConnectionsMap into Adjs
buildAdjs ::
  ItemConnectionsMap -- ^ all connections
  -> Item -- ^ starting item to compute adjacencies for
  -> M.Map Item Adjs -- ^ accumulated map of adjancies
  -> M.Map Item Adjs -- ^ new map of adjacencies
buildAdjs allConns item visited = if item `M.member` visited
  -- if we've been here before, then there's nothing to do
  then visited
  else r where
    -- list of our children
    children :: [Item]
    children = S.toList . M.keysSet $ M.findWithDefault M.empty item allConns
    -- add ourselves to visited nodes
    r0 = M.insert item ([], children) visited
    -- recursively compute Adjs for our children
    foldFn1 :: Item -> Map Item Adjs -> Map Item Adjs
    foldFn1 child acc = buildAdjs allConns child acc
    r1 = foldr foldFn1 r0 children
    -- finally add ourselves to each child as a parent
    foldFn2 :: Item -> Map Item Adjs -> Map Item Adjs
    foldFn2 child acc = M.alter (Just . over _1 (item:) . fromMaybe ([],[])) child acc
    r2 = foldr foldFn2 r1 children
    -- clear items we've visited already
    -- (inefficient but whatever)
    newAllConns = M.difference allConns r2
    -- recursively call this function if there's anything left
    -- why is there only a partial version of M.findMin -__-
    r = if M.null newAllConns
      then r2
      else buildAdjs newAllConns (fst $ M.findMin newAllConns) r2

-- per node accumulating data for evalTier
data NodeAcc = NodeAcc {
  adjs        :: Adjs
  , tierFn    :: TierFn -- not fully constructed until all children are done processing
  , tierThunk :: Int -- do not force this value!!
}

data CallerInfo = CallerInfo {
  calledFromChild :: Bool
  , callerTier    :: Maybe Int
  , caller        :: Item
}

mustFind :: (Ord k) => k -> M.Map k v -> v
mustFind = M.findWithDefault (error "key does not exist")


evalTier ::
  M.Map Item Int -- ^ forced tiers
  -> S.Set Item -- ^ processing nodes
  -> Maybe CallerInfo -- ^ caller info, Nothing if first call :(
  -> M.Map Item Adjs -- ^ accumulating Adjs (adjs depopulated as we go)
  -> M.Map Item NodeAcc -- ^ accumulating node info (populated as we go)
  -> Item -- ^ item we are evaluating
  -> (Maybe Int, M.Map Item Adjs, M.Map Item NodeAcc) -- ^ return value and accumulator
evalTier forced processing callerInfo adjsAcc1 nmAcc0 selfItem = rFinal where

  fTier = M.lookup item forced
  (parents, children) = M.findWithDefault ([],[]) item adjsAcc1

  -- add ourselves to the node accumulator map
  nmAcc1 = M.adjust (\acc -> acc {
      tierFn = emptyTierFn
      , tierThunk = tierFinal
    })

  -- add ourselves to processing (for children only)
  selfProcessing = (S.insert selfItem processing)

  removeSelfFromAdjs ::
    Lens' ([Item],[Item]) [Item] -- ^ lens to parent on children
    -> Item -- ^ item we want to remove self from
    -> M.Map Item Adjs
    -> M.Map Item Adjs
  removeSelfFromAdjs l item adjsAcc = M.adjust (over l (L.delete selfItem)) item adjsAcc

  -- mapAccum function we'll use on our children
  crfn ::
    (M.Map Item Adjs, M.Map Item NodeAcc)
    -> Item
    -> (Maybe Int, (M.Map Item Adjs, M.Map Item NodeAcc))
  crfn (adjsAcc, nmAcc) cItem
    -- (c1) already processing base case
    | S.member cItem selfProcessing =
      -- insert self into parent of cItem as Nothing to indicate fixed point of loop
      -- return Nothing to indicate loop (TODO return forced tier instead?)
      (Nothing, (adjsAccForChild, M.adjust (\acc -> acc { tierFn = over _1 (M.insert selfItem Nothing) (tierFn acc) }) cItem nmAcc))
    -- (c2) discovered and not processing base case
    | Just acc <- M.lookup cItem nmAcc =
      -- can safely use its tier thunk from the map
      (Just $ tierThunk nmAcc, (adjsAccForChild, nmAcc))
    -- (c3) recursive case
    | otherwise = (cTier, (newAdjsAcc, newAcc)) where
      -- remove self from parent adjs of child
      adjsAccForChild = removeSelfFromAdjs _1 cItem adjsAcc

      -- remove children and eventual parents from caller's tierFn
      loopedParents = fst (mustFind item newAcc) S.\\ fst (mustFind item nmAcc)
      tierForChild = flip fromMaybe fTier $ evalTierFn (over snd (M.delete cItem) . over fst (M.\\ loopedParents) $ tierFn)
      selfInfoForChild = CallerInfo false tierForChild selfItem
      -- recursively call with our modified accumulators
      (cTier, newAdjsAcc, newAcc) = evalTier forced selfProcessing selfInfoForChild adjsAccForChild nmAcc cItem

  -- handle children
  (childTiers, adjsAcc2, nmAcc2) = mapAccumR crfn (adjsAcc1, nmAcc1) children

  -- mapAccum function we'll use on our parents
  prfn ::
    (M.Map Item Adjs, M.Map Item NodeAcc)
    -> Item
    -> (Maybe Int, (M.Map Item Adjs, M.Map Item NodeAcc))
  prfn (adjsAcc, nmAcc) pItem
    -- (p1) if processing
    | S.member pItem processing =
      -- insert self into child of pItem as tier thunk, parents always depend on child, except in the case of (c1)
      -- return Nothing to indicate parent depends on child (TODO return forced tier instead?)
      (Nothing, (adjsAccForChild, M.adjust (\acc -> acc { tierFn = over _1 (M.insert selfItem tierForParent) (tierFn acc) }) cItem nmAcc))
    -- (p2) if already discovered
    | Just _ <- M.lookup pItem nmAcc = error "this should never happen"
    -- (p3) recursive case
    | otherwise = (pTier, (newAdjsAcc, newAcc)) where
      -- remove self from child adjs of parent
      adjsAccForParent = M.adjust (over _2 (L.delete selfItem)) cItem adjsAcc

      -- remove parent from tierFn we pass to parent (TODO not totally sure if we should do this but seems correct)
      tierForParent = flip fromMaybe fTier $ evalTierFn (over fst (M.delete pItem) tierFn)
      selfInfoForParent = CallerInfo true tierForParent selfItem
      -- recursively call with our modified accumulators
      (pTier, newAdjsAcc, newAcc) = evalTier forced processing selfInfoForParent adjsAccForParent nmAcc pItem

  -- handle parents
  (parentTiers, adjsAccFinal, nmAccFinal) = mapAccumR prfn (adjsAcc2, nmAcc2) parents


  tierFnFinal = (parentTiers, childTiers)
  -- TODO handle forced tiers
  tierFinal = evalTierFn tierFnFinal
  rTier = evalTierFnFixed tierFnFinal

  -- this step is not necessary, but it's useful for debugging if we have the fully constructed tierFn
  nmAccDebugFinal = M.adjust (\acc -> acc {tierFn = tierFnFinal }) nmAccFinal

  rFinal = (rTier, adjsAccFinal, nmAccFinal)


data TierFn = TierFn{
  parents    :: M.Map Item (Maybe Int)
  , children :: M.Map Item (Maybe Int)
}

emptyTierFn :: TierFn
emptyTierFn = TierFn M.empty M.empty

evalTierFn :: TierFn -> Int
evalTierFn (TierFn ps cs) = evalTierFn' (M.elems ps, M.elems cs)

-- | this version carefully opens loops so that there is a fixed point in our knot :)
evalTierFnFixed :: TierFn -> Maybe Int
evalTierFnFixed tfn@(TierFn ps cs) = if null cs
  then Just 0
  else if null ps
    -- if no parents, return  Nothing indicating our tier is dependent on children
    then Nothing
    else if all isNothing cs
      -- if there are children and they are all Nothing, return Nothing indicating we are in a loop
      then Nothing
      else if any isNothing ps
        then Nothing
        else Just $ evalTierFn tfn

-- | data type containing information to compute a node's tier
-- (parent tiers, child tiers)
type TierFn' = ([Maybe Int], [Maybe Int])

-- | convert a fully constructed TierFn to a node's actual tier
evalTierFn' :: TierFn' -> Int
evalTierFn' (ps, cs) = r where
  -- compute the min of two parent tiers
  minps :: Maybe Int -> Maybe Int -> Maybe Int
  --minps Nothing p2 = p2
  --minps p1 Nothing = p1
  minps p1 p2      = minOrd1 p1 p2
  -- compute the max of two child tiers
  maxcs :: Maybe Int -> Maybe Int -> Maybe Int
  maxcs = maxOrd1
  -- compute the new tier from min/max of parent/child tiers
  combine_minps_maxcs :: Maybe Int -> Maybe Int -> Int
  -- No fixed pt in both parent and child, this means we hit a loop, so our tier is 0
  combine_minps_maxcs Nothing Nothing       = 0
  -- Nothing in child means we're in a loop, use the same tier
  combine_minps_maxcs (Just mps) Nothing    = mps
  -- no parent tiers means we use the child tier plus 1
  combine_minps_maxcs Nothing (Just mcs)    = mcs + 1
  -- otherwise take the min (parent pushes children down, but no further than its children)
  combine_minps_maxcs (Just mps) (Just mcs) = max mcs $ min (mps - 1) (mcs + 1)
  r = if null cs
    -- if there are NO child nodes, we have no dependencies so our tier is 0
    then 0
    -- relude has no foldl1 for some reason, need lazy variant so whatever we just use partial functions here it's fine go away
    else if null ps
      then combine_minps_maxcs Nothing (L.foldr1 maxcs cs)
      else combine_minps_maxcs (L.foldr1 minps ps) (L.foldr1 maxcs cs)


-- TODO TEST
-- | returns the tier of the item according to rules (see README.md)
findTiers ::
  ItemConnectionsMap -- ^ all connections
  -> M.Map Item Int -- ^ known or forced tiers
  -> Item -- ^ starting item
  -> M.Map Item Int -- ^ all item tiers
findTiers allConns forced item = traceShowId tiers where
  -- first build adjacency map
  --adjs = trace "HI:" $ trace_pShow allConns $ traceShowId $ buildAdjs allConns item M.empty
  adjs = buildAdjs allConns item M.empty
  -- next evaluate tiers
  (tiers', _) = evalTier forced adjs Nothing S.empty M.empty item

  tiers = trace ("FOUND TIERS STARTING WITH: " <> show item <> " TIERS:" <> (show $ M.map fst tiers')) $ M.map snd tiers'


newGenerateTieredItems ::
  ItemSet -- ^ all items
  -> RecipeSet -- ^ all recipes
  -> M.Map Int ItemSet -- ^ map of tiers to item in each tier
newGenerateTieredItems items recipes = tierToItem where
  allConns = mapSetToMap (findItemConnections recipes) items

  -- | keep finding tiers of items until we've found all
  untilEmpty :: ItemSet -> M.Map Item Int -> M.Map Item Int
  untilEmpty rest known = if S.null rest then trace ("until empty done" <> show (M.size known)) $ known else r where
    -- take the first item, (must exist since set is non empty)
    item = trace ("until empty") $ S.findMin rest
    newKnown = findTiers allConns M.empty item `M.union` known
    newRest = rest S.\\ M.keysSet newKnown
    -- Nothing means circular dependency with no other dependencies which is tier 0
    r = untilEmpty newRest newKnown

  itemToTier = untilEmpty items M.empty

  -- reverse keys/values
  foldfn :: Item -> Int -> M.Map Int ItemSet -> M.Map Int ItemSet
  foldfn item t acc =  M.insert t (item `S.insert` M.findWithDefault S.empty t acc) acc
  tierToItem = M.foldrWithKey foldfn M.empty itemToTier

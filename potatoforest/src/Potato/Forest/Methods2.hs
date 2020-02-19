{-# LANGUAGE PatternGuards #-}
module Potato.Forest.Methods2 (
  findTiers
  , newGenerateTieredItems

  -- exported for testing
  , evalTierFn
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

clearInTuple ::
  Lens' ([Item],[Item]) [Item]
  -> Item -- ^ item to clear
  -> Item -- ^ item we are clearing from
  -> M.Map Item Adjs -- ^ map of adjacencies
  -> M.Map Item Adjs -- ^ map with item removed from parents of all of item's children
clearInTuple l toClear clearFrom adjs = M.adjust (over l (L.delete toClear)) clearFrom adjs

clearItemFromChildren :: Item -> M.Map Item Adjs -> M.Map Item Adjs
clearItemFromChildren parent adjs = case M.lookup parent adjs of
  Nothing      -> adjs
  -- go through children and remove parent
  Just (_, cs) -> foldr (clearInTuple _1 parent) adjs cs

clearItemFromParents :: Item -> M.Map Item Adjs -> M.Map Item Adjs
clearItemFromParents child adjs = case M.lookup child adjs of
  Nothing      -> adjs
  -- go through parents and remove child
  Just (ps, _) -> foldr (clearInTuple _2 child) adjs ps

--thunkTierFn :: TierFn -> (TierFn, Int)
--thunkTierFn tfn = (tfn, evalTierFn tfn)

-- does not evaluate tier thunks within
showDebugTierFn :: TierFn -> String
showDebugTierFn (a,b) = show (fmap isNothing a, fmap isNothing b)


-- | evaluate the tier of an item
-- returns (tier, new map with tier inserted)
-- fixed version of above
evalTier ::
  M.Map Item Int -- ^ forced tiers
  -> M.Map Item Adjs -- ^ map of adjancencies made in the previous step
  -> Maybe (Either (Maybe Int) Int) -- ^ left is parent of this item's tier, right is child of this item's tier
  -> S.Set Item -- ^ set of visited nodes that are still being processed
  -- TODO do not need full TierFn, just list of parent tiers
  -> M.Map Item (TierFn, Int) -- ^ accumulating map of node tiers and their (possibly still being constructed TierFn)
  -> Item -- ^ current item we're processing
  -- TODO get rid of either
  -- TODO you can even get rid of maybe if you check if children is processing first before recursing in children
  -> (M.Map Item (TierFn, Int), Either (Maybe Int) (Maybe Int)) -- ^ (new accumulating map, our own tier (left if just parent or child, right if child and eventual parent, Nothing if tier is dependent on caller's tier)
evalTier forced adjs mct process disc item
  -- we're still processing this node
  | item `S.member` process = trace ("hit processing: " <> show item) $ case mct of
    Nothing -> error "this should never happen, recursive calls always have caller"
    Just ct -> case ct of
      -- called from parent, add Nothing to TierFn indicating loop
      -- give back Nothing,
      Left pTier -> r where
        r = (M.adjust (over (_1 . _1) (Nothing:)) item disc, Right Nothing)
      -- called from child
      -- fixed point in looping cases are handled in children which we recurse over first
      -- so we can safely use tier thunk in the map which must exist
      Right _ -> (disc, Left . Just . snd $ M.findWithDefault (error "this should never happen") item disc)
  -- we're done processing children of this node so we aren't in a circular loop case
  | Just (_,tier) <- M.lookup item disc = trace ("hit disc: " <> show item) $ case mct of
      Nothing -> error "this should never happen, recursive calls always have caller"
      Just ct -> case ct of
        -- called from parent, add parents tier to TierFn
        Left pTier -> (M.adjust (over (_1 . _1) (pTier:)) item disc, Left (Just tier))
        -- called from child, just return the answer
        -- note this closes loops that were found when we go down children
        Right _    -> (disc, Right (Just tier))
  | otherwise = trace ("hit standard: " <> show item) $ r where
    -- remove self as parent from children first because this node may be its own parent (which causes an infinite loop)
    adjsC = clearItemFromChildren item adjs
    -- (it should always exist in the map assuming buildAdjs works correctly)
    (ps, cs) = M.findWithDefault ([],[]) item adjsC
    -- put our to-be-constructed TierFn and a thunk to our own tier in disc
    d0 = trace ("eval tier " <> show item <> " " <> show (ps, cs)) $ M.insert item (([],[]), tier) disc
    --d0 = trace ("eval tier " <> show item <> " " <> show (ps, cs)) $ M.insert item (([],[]), tier) disc

    crfn ::
      M.Map Item (TierFn, Int) -- ^ accumulating map of node tiers and their (possibly still being constructed TierFn)
      -> Item -- ^ current item we're processing
      -> (M.Map Item (TierFn, Int), Either (Maybe Int) (Maybe Int))
    crfn acc cItem = crfnr where
      cProcess = (S.insert item process)
      (newAcc, ct) = if cItem `S.member` cProcess
        -- if child is processing
        -- add Nothing to the parent of cItem indicating we are in a loop
        then trace ("hit processing: " <> show cItem) $
          (M.adjust (over (_1 . _1) (Nothing:)) cItem acc, Right Nothing)
        else if cItem `M.member` acc
          -- if child is discovered, (this means we're done processing children of this node so we aren't in a circular loop case)
          -- add our tier thunk into the parent of cItem
          then trace ("hit disc: " <> show cItem) $
            (M.adjust (over (_1 . _1) (forChildrenTier:)) cItem acc, Left (Just tier))
          else evalTier forced adjsC (Just $ Left forChildrenTier) cProcess acc cItem
      -- TODO check if M.lookup item newAcc is different from
      crfnr = (newAcc, ct)

    -- recursively call in all children, add self to processing nodes before calling
    (d1, csts'') = mapAccumR crfn d0 cs

    -- remove self from parents before recursing
    adjsP = (clearItemFromParents item adjsC)

    prfn ::
      M.Map Item (TierFn, Int) -- ^ accumulating map of node tiers and their (possibly still being constructed TierFn)
      -> Item -- ^ current item we're processing
      -> (M.Map Item (TierFn, Int), Either (Maybe Int) (Maybe Int))
    prfn acc pItem = prfnr where
      (newAcc, pt) = if pItem `S.member` process
        -- if parent is processing
        -- fixed point in looping cases are handled in children which we recurse over first
        -- so we can safely use tier thunk in the discovered map which must exist
        then trace ("hit processing: " <> show pItem) $
          (acc, Left . Just . snd $ M.findWithDefault (error "this should never happen") pItem acc)
        else if pItem `M.member` acc
          -- if parent is discovered, (this means we're done processing children of this node so we aren't in a circular loop case)
          -- just return the tier
          -- note this closes loops that were found when we go down children
          then trace ("hit disc: " <> show pItem) $
            (acc, Right (Just tier))
          else evalTier forced adjsP (Just $ Right tier) process acc pItem
      -- TODO check if M.lookup item newAcc is different from
      prfnr = (newAcc, pt)

    -- recursively call in all parents, pass in a our own tier as a thunk
    (d2, psts'') = mapAccumR prfn d1 ps

    -- add caller's tier to parent or children
    (psts', csts') = fromMaybe (psts'', csts'') $ flip fmap mct $ \case
      Left pTier -> (Left pTier:psts'',csts'')
      Right cTier -> (psts'', Left (Just cTier):csts'')

    -- keep both left and right for children
    csts = map (either id id) csts'
    -- keeps only lefts, the rights are included in d2
    psts = lefts psts'

    -- collect anything added to our TierFn while traversing children
    ((psts2, csts2), _) = M.findWithDefault (error "this should never happen") item d2
    tierFn@(rpsts,rcsts) = (psts2<>psts, csts2<>csts)

    -- compute our actual tier, and then the tier we will pass to our parents/children
    tier = evalTierFn tierFn
    --trace ("evalTier rslt: " <> show item <> " " <> show (fmap isNothing rpsts, fmap isNothing rcsts))
    rTier = if null rcsts
      then Left (Just 0) -- needed to break loops, even though it's the same as `Left (Just tier)`
      else if null rpsts || any isNothing rpsts
        -- if no parents or any parent is Nothing, return Left Nothing indicating our tier is dependent on children
        then trace ("left nothing: " <> show item) $ Left Nothing
        else if all isNothing rcsts
          -- if there are children and they are all Nothing, return Right Nothing indicating we are in a loop
          then Right Nothing
          else Left (Just tier)
    -- the tier we report when recursively calling on our children
    forChildrenTier = either id id rTier
    r = (d2, rTier)


{- can't remember why I did this, you can delete it but here it is just in case
else if all isRight csts'
  -- if there are children and they are all Right, return Right indicating we are in a loop
  -- our tier is the maximal of all children tiers (some may have been forced)
  then (d2, Right (L.foldr1 maxOrd1 csts))
-}


-- | data type containing information to compute a node's tier
-- (parent tiers, child tiers)
type TierFn = ([Maybe Int], [Maybe Int])

-- | convert a fully constructed TierFn to a node's actual tier
evalTierFn :: TierFn -> Int
evalTierFn (ps, cs) = r where
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
  tiers = M.map snd tiers'


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

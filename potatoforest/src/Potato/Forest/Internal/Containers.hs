
module Potato.Forest.Internal.Containers (
  mapSetToMap
  , map_keys_forM
  , lookup
) where

import Relude

-- importing internal gives us access too Map and Set ctors
import qualified Data.Map.Internal as M
import qualified Data.Set.Internal as S


-- | converts a set to map
-- TODO move this into a different module so we don't have to import internal
mapSetToMap :: (a -> b) -> S.Set a -> M.Map a b
mapSetToMap _ S.Tip                = M.Tip
mapSetToMap f (S.Bin sz elt ls rs) = M.Bin sz elt (f elt) (mapSetToMap f ls) (mapSetToMap f rs)


-- something like this instead, but need to flip args to define functor instance
--newtype KeyMap a b = KeyMap { unKeyMap :: M.Map a b }
map_keys_forM :: (Monad m) => M.Map k a -> (k -> m b) -> m [b]
map_keys_forM m f = M.foldrWithKey (\k _ acc -> (:) <$> f k <*> acc) (return []) m

-- Notably, the method Relude.Extra.Map.lookup just returns the argument, not the item contained in the set
lookup :: Ord a => a -> S.Set a -> Maybe a
lookup !_ S.Tip = Nothing
lookup x (S.Bin _ y l r)
  | x == y = Just y
  | x < y = lookup x l
  | otherwise = lookup x r

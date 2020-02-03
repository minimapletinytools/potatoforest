
module Potato.Forest.Internal.Containers (
  mapSetToMap
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

lookup :: Ord a => a -> S.Set a -> Maybe a
lookup !_ S.Tip = Nothing
lookup x (S.Bin _ y l r)
  | x == y = Just y
  | x < y = lookup x l
  | otherwise = lookup x r

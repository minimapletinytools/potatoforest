--{-# LANGUAGE RecursiveDo     #-}
module Widgets (
  itemSize
  , offsetPosToItemBoxCenter
  , ItemAttr(..)
  , toAttrMap
  , ItemMeta(..)
  , ItemMetaMap
  , LineMeta(..)
  , line
  , itemBox
) where

import qualified Data.Map            as M
import           Relude

import           Helper

import           Potato.Forest.Types
import           Reflex.Dom


-- | size of an item box, make sure this matches CSS
itemSize :: Pos
itemSize = (80,100)

offsetPosToItemBoxCenter :: Pos -> Pos
offsetPosToItemBoxCenter (a,b) = (a + fst itemSize `div` 2, b + snd itemSize `div` 2)

-- | item attribute data
-- TODO delete this
data ItemAttr = ItemAttr {
  ia_pos :: Pos
}

-- | convert ItemAttr to attributes understood by reflex
toAttrMap :: ItemAttr -> Map Text Text
toAttrMap ia = fromList [
    ("style", style)
    , ("class", "tech")
  ] where
    style =
      "left:" <> (show . fst) (ia_pos ia) <> "px;"
      <> "top:" <> (show . snd) (ia_pos ia) <> "px;"

-- | reflex item meta data
data ItemMeta t = ItemMeta {
  im_item              :: Item
  , im_attr            :: ItemAttr -- TODO rename this to im_pos, this is confusing since there is also dynamic attribute on this elt
  , im_trigger_action  :: Map Text Text -> IO()
  , im_click_event     :: Event t ()
  , im_mouseover_event :: Event t ()
  , im_mouseout_event  :: Event t ()
}

-- | map of items to their metadata
type ItemMetaMap t = Map ItemId (ItemMeta t)

data LineMeta = LineMeta {
  lm_trigger_action :: Map Text Text -> IO ()
}

-- | draw line between two items (itemel1, itemel2)
-- thx https://github.com/kouky/line-css.js/blob/master/src/line-css.coffee
line :: (MonadWidget t m) => Pos -> Pos -> m LineMeta
line a b = do
  let
    attrMap = M.fromList [
         styleToAttr (makeLineStyle a b)
        , ("class", "path hidden")
      ]
  (modAttrEv, action) <- newTriggerEvent
  dynAttrs <- foldDyn M.union attrMap modAttrEv
  (e,_) <- elDynAttr' "div" dynAttrs blank
  return $ LineMeta {
      lm_trigger_action = action
    }

-- | creates a widget for an item and returns its ItemMeta
itemBox :: (MonadWidget t m) => Item -> ItemAttr -> m (ItemMeta t)
itemBox item attr = do
  (modAttrEv, action) <- newTriggerEvent
  -- modAttrEv produces map of new attributes which we union with previous ones
  dynAttrs <- foldDyn M.union (toAttrMap attr) modAttrEv
  (e, _) <- elDynAttr' "div" dynAttrs $ el "p" $ text (unItemId (itemId item))
  return $ ItemMeta {
    im_item = item
    , im_attr = attr
    , im_trigger_action = action
    , im_click_event = domEvent Click e
    , im_mouseover_event = domEvent Mouseover e
    , im_mouseout_event = domEvent Mouseout e
  }

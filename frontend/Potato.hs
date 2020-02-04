{-# LANGUAGE RecursiveDo #-}


module Potato (
  potatomain
) where

import           Relude

import           Potato.Forest.Methods
import           Potato.Forest.Parser
import           Potato.Forest.Types
import           Reflex.Dom


import           Data.Text             (pack, unpack)

-- build a map of item to its event trigger function thingy, location, item info etc...

data ItemMeta t = ItemMeta {
  im_item              :: Item
  , im_attr            :: ItemAttr
  , im_trigger_action  :: Map Text Text -> IO()
  , im_click_event     :: Event t ()
  , im_mouseover_event :: Event t ()
  , im_mouseout_event  :: Event t ()
}

data ItemAttr = ItemAttr {
  ia_pos :: (Int, Int)
}

toAttrMap :: ItemAttr -> Map Text Text
toAttrMap ia = fromList [
  ("style", style)
  ] where
    style =
      "left:" <> (show . fst) (ia_pos ia) <> "px;"
      <> "top:" <> (show . snd) (ia_pos ia) <> "px;"
      <> "position: absolute;"
      <> "z-index: 1;"
      <> "width: 80px;"
      <> "cursor: pointer;"
      <> "border-color: green"

type ItemMetaMap = Map ItemId Item

-- draw line between two items (itemel1, itemel2)

itemBox :: forall t m a. (MonadWidget t m) => Item -> ItemAttr -> m (ItemMeta t)
itemBox item attr = do
  (modAttrEv, action) <- newTriggerEvent
  dynAttrs <- holdDyn (toAttrMap attr) (modAttrEv)
  (e, _) <- elDynAttr' "div" dynAttrs $ text (unItemId (itemId item))
  return $ ItemMeta {
    im_item = item
    , im_trigger_action = action
    , im_click_event = domEvent Click e
    , im_mouseover_event = domEvent Mouseover e
    , im_mouseout_event = domEvent Mouseout e
  }

potatomain :: IO ()
potatomain = do
  spec <- readFile "testing2.spec"
  let
    Right fb = runForestBlocksParser (pack spec)
    tiered = generateTieredItems (knownItems fb) (knownRecipes fb)
  mainWidget $ el "div" $ do
    el "div" $ text (pack (show tiered))
    --el "div" $ text (pack spec)
    metas <- el "ul" $ forM [0..10] $ \x -> itemBox builtin_time (ItemAttr (x*200, x*100))
    el "div" $ text helloPotato
    el "div" $ do
      t <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      dynText $ _inputElement_value t

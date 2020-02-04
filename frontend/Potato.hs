{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}


module Potato (
  potatomain
) where

import qualified Data.Map              as M
import           Relude
import           Relude.Extra.Map      (lookup)

import           Potato.Forest.Methods
import           Potato.Forest.Parser
import           Potato.Forest.Types
import           Reflex.Dom

import           Data.FileEmbed


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
  , ("class", "tech")
  ] where
    style =
      "left:" <> (show . fst) (ia_pos ia) <> "px;"
      <> "top:" <> (show . snd) (ia_pos ia) <> "px;"


type ItemMetaMap t = Map ItemId (ItemMeta t)

-- draw line between two items (itemel1, itemel2)

itemBox :: (MonadWidget t m) => Item -> ItemAttr -> m (ItemMeta t)
itemBox item attr = do
  (modAttrEv, action) <- newTriggerEvent
  -- modAttrEv produces map of new attributes which we union with previous ones
  dynAttrs <- foldDyn M.union (toAttrMap attr) modAttrEv
  (e, _) <- elDynAttr' "div" dynAttrs $ text (unItemId (itemId item))
  return $ ItemMeta {
    im_item = item
    , im_trigger_action = action
    , im_click_event = domEvent Click e
    , im_mouseover_event = domEvent Mouseover e
    , im_mouseout_event = domEvent Mouseout e
  }

mapWithIndexM :: (Traversable t, Monad m) => (Int -> a -> m b) -> t a -> m (t b)
mapWithIndexM f ta = sequence $ snd (mapAccumL (\i a -> (i+1, f i a)) 0 ta)

potatomain :: IO ()
potatomain = do
  let
    css = $(embedStringFile "potato.css")
    spec = $(embedStringFile "testing2.spec")

    Right fb = runForestBlocksParser (pack spec)
    tiered = sortTieredItems $ generateTieredItems (knownItems fb) (knownRecipes fb)

    innerMap :: (MonadWidget t m) => Int -> Int -> (Item, ItemConnections) -> m (ItemMeta t)
    innerMap tier x (item, _) = itemBox item attrs where
      pos = (x * 100 + 100, tier * 100 + 100)
      attrs = ItemAttr { ia_pos = pos }

    outerMap :: (MonadWidget t m) => Int -> ItemConnectionsList -> m ([ItemMeta t])
    outerMap tier items = mapWithIndexM (innerMap tier) items

  mainWidgetWithCss css $ do
  --mainWidget $ do
    itemMetas' <- mapWithIndexM outerMap tiered
    let
      itemMetas :: [ItemMeta (SpiderTimeline Global)]
      itemMetas = join itemMetas'
      itemMetaMap = M.fromList $ map (\im -> (itemId (im_item im), im)) itemMetas

      -- for now, we just highlight ourself
      makeTriggerClick :: ItemId -> IO ()
      makeTriggerClick iid = case lookup iid itemMetaMap of
        Nothing -> error "item not found"
        Just im -> (im_trigger_action im) ("class" =: "tech selected")

      simpleTrigger :: ItemMeta t -> IO()
      simpleTrigger im = do
        putStrLn $ "you clicked " ++ show (im_item im)
        (im_trigger_action im) ("class" =: "tech selected")

    forM_ itemMetas (\im -> performEvent ((\_ -> liftIO $ simpleTrigger im) <$> im_click_event im))
    return ()

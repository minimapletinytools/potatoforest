{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}


module Potato (
  potatomain
) where

import qualified Data.Map              as M
import           Numeric               (showFFloat)
import           Prelude               (atan2)
import           Relude
import           Relude.Extra.Map      (lookup)
import qualified Relude.Unsafe         as RU

import           Helper
import           Widgets

import           Potato.Forest.Methods
import           Potato.Forest.Parser
import           Potato.Forest.Types
import           Reflex.Dom

import           Data.FileEmbed


import qualified Data.Text             as T


potatomain :: IO ()
potatomain = do
  let
    css = $(embedStringFile "potato.css")
    spec = $(embedStringFile "testing2.spec")

    -- parse potato
    Right fb = runForestBlocksParser (T.pack spec)
    tiered' = generateTieredItems (knownItems fb) (knownRecipes fb)
    allConnections = foldr M.union M.empty tiered'
    tiered = sortTieredItems $ tiered'

    -- | helper method for rendering items
    innerMap :: (MonadWidget t m) => Int -> Int -> (Item, ItemConnections) -> m (ItemMeta t)
    innerMap tier x (item, _) = itemBox item attrs where
      pos = (x * 150 + 100, tier * 200 + 100)
      attrs = ItemAttr { ia_pos = pos }

    -- | helper method for rendering items
    outerMap :: (MonadWidget t m) => Int -> ItemConnectionsList -> m ([ItemMeta t])
    outerMap tier items = mapWithIndexM (innerMap tier) items

    -- TODO add actions for highlighting
    -- | helper method for rendering lines
    lineMap :: (MonadWidget t m) =>
      ItemMetaMap t -- ^ all items
      -> ItemConnectionsMap -- ^ map to look up ItemConnections
      -> ItemMeta t -- ^ item we want to draw all lines from
      -> m ()
    lineMap imm icm im = do
      let
        pos' = ia_pos . im_attr $ im
        connections :: ItemConnections
        connections = M.findWithDefault M.empty (im_item im) icm
      mapKeysM connections $ \item -> do
        let
          maybeEndItemMeta = lookup (itemId item) imm
        case maybeEndItemMeta of
          Nothing          -> blank
          Just endItemMeta -> line (offsetPosToItemBoxCenter pos') (offsetPosToItemBoxCenter . ia_pos . im_attr $ endItemMeta)
      return ()

    -- main widget
    potatoWidget :: forall t m. (MonadWidget t m) => m ()
    potatoWidget = mdo

      -- generate item boxes
      itemMetas' <- mapWithIndexM outerMap tiered

      let
        -- list of all ItemMeta returned by our itemBox widget
        itemMetas :: [ItemMeta t]
        itemMetas = join itemMetas'

        -- same as above but in map form
        itemMetaMap :: Map ItemId (ItemMeta t)
        itemMetaMap = M.fromList $ map (\im -> (itemId (im_item im), im)) itemMetas

        --
        withItemMeta :: ItemId -> a -> (ItemMeta t -> a) -> a
        withItemMeta iid n jf = case lookup iid itemMetaMap of
          Nothing -> n
          Just i  -> jf i

        -- | recursively selects an items and all items it depends on
        selectRecursive ::
          ItemMetaMap t -- ^ all items
          -> ItemConnectionsMap  -- ^ map to look up ItemConnections
          -> ItemMeta t -- ^ the selected item in question
          -> IO ()
        selectRecursive imm icm im = do
          let
            connections = M.findWithDefault M.empty (im_item im) icm
          mapKeysM connections $ \item -> do
            let
              maybeEndItemMeta = lookup (itemId item) imm
            withItemMeta (itemId item) blank (selectRecursive imm icm)
          im_trigger_action im ("class" =: "tech selected")

        -- recursively unselects an items and all items it depends on
        unselectRecursive ::
          ItemMetaMap t -- ^ all items
          -> ItemConnectionsMap  -- ^ map to look up ItemConnections
          -> ItemMeta t -- ^ the selected item in question
          -> IO ()
        unselectRecursive imm icm im = do
          let
            connections = M.findWithDefault M.empty (im_item im) icm
          mapKeysM connections $ \item -> do
            let
              maybeEndItemMeta = lookup (itemId item) imm
            case maybeEndItemMeta of
              Nothing          -> blank
              Just endItemMeta -> unselectRecursive imm icm endItemMeta
          im_trigger_action im ("class" =: "tech")

        -- for now, we just highlight ourself
        simpleTrigger :: ItemMeta t -> IO ()
        simpleTrigger im = do
          putStrLn $ "you clicked " ++ show (im_item im)
          im_trigger_action im ("class" =: "tech selected")

        -- action to select an item
        selectAction :: Maybe (ItemMeta t) -> ItemMeta t -> IO ()
        selectAction prev im = do
          -- first unselect previous
          case prev of
            Nothing  -> return ()
            Just pim -> trace ("unselecting: " <> show (im_item pim)) $ unselectRecursive itemMetaMap allConnections pim
          -- then select current
          trace ("selecting: " <> show (im_item im)) $ selectRecursive itemMetaMap allConnections im

        -- TODO finish
        hoverWidget :: (MonadWidget t m)
          => ItemMeta t -- ^ the item we are hovering over
          -> m ()
        hoverWidget im = do
          let
            attrs = M.fromList [("id", "tooltip2"), styleToAttr (posToStyle . offsetPosToItemBoxCenter $ (ia_pos . im_attr $ im))]
          liftIO $ print $ "HOVERING: " ++ show (im_item im)
          elAttr "div" attrs $ el "p" (text "this is a tooltip")

        -- helper, TBH only needed to disambiguate the type var t
        hoverWidgetEv :: (MonadWidget t m)
          => Event t (Maybe (ItemMeta t)) -- ^ the item we are hovering over
          -> Event t (m ())
        hoverWidgetEv ev = fmap f ev where
          f mim = case mim of
            Nothing -> blank
            Just ev -> hoverWidget ev
      --end let

      -- variable to track current selection (clickItemEv defined later on)
      currentSelection :: Behavior t (Maybe (ItemMeta t)) <- hold Nothing clickItemEv

      -- set up on click triggers
      --forM_ itemMetas (\im -> performEvent ((\_ -> liftIO $ simpleTrigger im) <$> im_click_event im))
      clickItemEvs_ <- forM itemMetas (\im -> performEvent
        ((\prev -> liftIO $ selectAction prev im >> return im) <$> tag currentSelection (im_click_event im)))

      let
        -- create an event when we mouse over any item
        hoverEv :: Event t (Maybe (ItemMeta t)) = leftmost $ map (\im -> fmap (const (Just im)) (im_mouseover_event im)) itemMetas

        -- create an event that fires after any of the actions above is performed
        clickItemEv :: Event t (Maybe (ItemMeta t)) = Just <$> leftmost clickItemEvs_

      -- set up hover widget events
      widgetHold blank $ hoverWidgetEv hoverEv

      -- draw lines
      forM_ itemMetas (lineMap itemMetaMap allConnections)

      return ()


  -- actually render stuff now
  mainWidgetWithCss css $ do potatoWidget
  --mainWidget $ do

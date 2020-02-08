{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}


module Potato (
  potatomain
) where

import qualified Data.Map              as M
import qualified Data.Set              as S
import           Relude
import           Relude.Extra.Map      (lookup)

import           Helper
import           Widgets

import           Potato.Forest.Methods
import           Potato.Forest.Parser
import           Potato.Forest.Types
import           Reflex.Dom

import           Data.FileEmbed


import qualified Data.Text             as T


-- | helper method
withItemMeta ::
  ItemMetaMap t -- ^ all know items
  -> ItemId -- ^ ItemId of item we want to operate on
  -> a -- ^ result if item is not found
  -> (ItemMeta t -> a) -- ^ function to apply to found ItemMeta
  -> a
withItemMeta imm iid n jf = case lookup iid imm of
  Nothing -> n
  Just i  -> jf i

-- | traverse over items and its connections visiting each item only once collecting results
itemTraverse ::
  forall f t b. (Applicative f)
  => ItemMetaMap t -- ^ all items
  -> ItemConnectionsMap  -- ^ map to look up ItemConnections
  -> (ItemMeta t -> f b) -- ^ action to apply
  -> ItemMeta t -- ^ the starting item
  -> f [b] -- ^ collected results of our actions
itemTraverse imm icm f im' = traverse f (toList $ next S.empty (im_item im')) where
  next :: Set (ItemMeta t) -> Item -> Set (ItemMeta t)
  next visited item = case M.lookup (itemId item) imm of
      -- didn't find the item, go home (this should never happen)
      --Nothing -> visited
      Nothing -> error $ "item not found in imm map " <> (show item)
      Just im -> if im `S.member` visited
        -- if we visited this node already, go home
        then visited
        -- otherwise, iterate through all its children after adding itself to the set of visited nodes
        else S.foldl next (S.insert im visited) reqs where
          reqs = case M.lookup (im_item im) icm of
            -- no connections means nothing to fold over
            Nothing -> S.empty
            -- otherwise, we fold over the key set
            Just ci -> M.keysSet ci



potatomain :: IO ()
potatomain = do
  let
    css = $(embedStringFile "potato.css")
    spec = $(embedStringFile "testing2.spec")

    -- parse potato
    Right fb = runForestBlocksParser (T.pack spec)

    tiered' = generateTieredItems (knownItems fb) (knownRecipes fb)

    allConnections :: ItemConnectionsMap
    allConnections = foldr M.union M.empty tiered'

    tiered :: [ItemConnectionsList]
    tiered = sortTieredItems $ tiered'

    -- | helper method for rendering items
    innerMap :: (MonadWidget t m) => Int -> Int -> (Item, ItemConnections) -> m (ItemMeta t)
    innerMap tier x (item, _) = itemBox item attrs where
      pos = (x * 150 + 100, tier * 200 + 100)
      attrs = ItemAttr { ia_pos = pos }

    -- | helper method for rendering items
    outerMap :: (MonadWidget t m) => Int -> ItemConnectionsList -> m [ItemMeta t]
    outerMap tier items = mapWithIndexM (innerMap tier) items

    -- TODO add actions for highlighting
    -- | helper method for rendering lines
    lineMap :: (MonadWidget t m) =>
      ItemMetaMap t -- ^ all items
      -> ItemConnectionsMap -- ^ map to look up ItemConnections
      -> ItemMeta t -- ^ item we want to draw all lines from
      -> m [LineMeta] -- ^ list of all lines we created
    lineMap imm icm im = do
      let
        pos' = ia_pos . im_attr $ im
        connections = M.findWithDefault M.empty (im_item im) icm
      -- for now, join all connections together, (rather than discern by recipe)
      fmap join $ mapKeysM connections $ \item -> withItemMeta imm (itemId item) (return []) $ \eim -> do
        il <- line (offsetPosToItemBoxCenter pos') (offsetPosToItemBoxCenter . ia_pos . im_attr $ eim)
        return [il]

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

        -- | map of items to all lines going to it
        itemLineMetaMap :: Map ItemId [LineMeta]
        itemLineMetaMap = M.fromList itemLineMetas'

        -- | recursively selects an items and all items it depends on
        selectRecursive ::
          ItemMetaMap t -- ^ all items
          -> ItemConnectionsMap  -- ^ map to look up ItemConnections
          -> ItemMeta t -- ^ the selected item in question
          -> IO ()
        selectRecursive imm icm im = do
          -- recursively select over all connections
          let
            connections = M.findWithDefault M.empty (im_item im) icm
            lms = M.findWithDefault [] (itemId . im_item $ im) itemLineMetaMap
          mapKeysM connections $ \item ->
            withItemMeta imm (itemId item) blank (selectRecursive imm icm)
          -- select the current item
          im_trigger_action im ("class" =: "tech selected")
          forM_ lms (\lm -> lm_trigger_action lm ("class" =: "path"))

        -- recursively unselects an items and all items it depends on
        unselectRecursive ::
          ItemMetaMap t -- ^ all items
          -> ItemConnectionsMap  -- ^ map to look up ItemConnections
          -> ItemMeta t -- ^ the selected item in question
          -> IO ()
        unselectRecursive imm icm im = do
          -- recursively unselect over all connections
          let
            connections = M.findWithDefault M.empty (im_item im) icm
            lms = M.findWithDefault [] (itemId . im_item $ im) itemLineMetaMap
          mapKeysM connections $ \item ->
            withItemMeta imm (itemId item) blank (unselectRecursive imm icm)
          -- unselect the current item
          im_trigger_action im ("class" =: "tech")
          forM_ lms (\lm -> lm_trigger_action lm ("class" =: "path hidden"))


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
            Just pim -> unselectRecursive itemMetaMap allConnections pim
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

      -- set up on click triggers (currentSelection defined later)
      --forM_ itemMetas (\im -> performEvent ((\_ -> liftIO $ simpleTrigger im) <$> im_click_event im))
      clickItemEvs_ <- forM itemMetas (\im -> performEvent
        ((\prev -> liftIO $ selectAction prev im >> return im) <$> tag currentSelection (im_click_event im)))

      let
        -- create an event when we mouse over any item
        mouseoverEv :: Event t (ItemMeta t) = leftmost $ map im_mouseover_event itemMetas

        -- create an event when we mouse out any item
        mouseoutEv :: Event t (ItemMeta t) = leftmost $ map im_mouseout_event itemMetas

        -- create an event that fires after any of the actions above is performed
        clickItemEv :: Event t (ItemMeta t) = leftmost clickItemEvs_

      -- variable to track current selection
      currentSelection :: Behavior t (Maybe (ItemMeta t)) <- hold Nothing (Just <$> clickItemEv)

      -- set up hover widget events
      widgetHold blank $ hoverWidgetEv (leftmost [Just <$> mouseoverEv, const Nothing <$> mouseoutEv])

      -- draw lines and set up line map
      itemLineMetas' <- forM itemMetas $ \im -> do
        ils <- lineMap itemMetaMap allConnections im
        return (itemId . im_item $ im, ils)


      return ()


  -- actually render stuff now
  mainWidgetWithCss css $ do potatoWidget
  --mainWidget $ do

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
import           Relude.Unsafe         (fromJust)

import           Potato.Forest.Methods
import           Potato.Forest.Parser
import           Potato.Forest.Types
import           Reflex.Dom

import           Data.FileEmbed


import qualified Data.Text             as T


type Pos = (Int, Int)

-- | item attribute data
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
  , im_attr            :: ItemAttr
  , im_trigger_action  :: Map Text Text -> IO()
  , im_click_event     :: Event t ()
  , im_mouseover_event :: Event t ()
  , im_mouseout_event  :: Event t ()
}

-- | map of items to their metadata
type ItemMetaMap t = Map ItemId (ItemMeta t)

-- | these represent key value pairs of the "style" property of a tag
type Style = Map Text Text

-- | convert style into an attribute
styleToAttr :: Style -> (Text,Text)
styleToAttr style = ("style", style') where
  style' = T.intercalate ";" $ M.foldrWithKey (\k s (acc :: [Text]) -> (k <> ":" <> s) : acc ) [] style

{- fixme
toStyle :: (Text, Text) -> Maybe Style
toStyle style = maybeStyle where
  styles = map recover (T.splitOn ";" style)
  maybeStyle = case any isNothing style of
    True  -> Nothing
    False -> Just $ M.fromList (catMaybes styles)
  recover s = r where
    s' = T.splitOn ":" s
    r = do
      r1 <- s' !!? 0
      r2 <- s' !!? 1
      return (r1, r2)
-}

-- | generates style for a line from a to b
makeLineStyle :: Pos -> Pos -> Style
makeLineStyle a b = M.fromList [
    ("left",show x <> "px")
    , ("top", show y <> "px")
    , ("width", show (round hypotenuse) <> "px")
    , ("height", show stroke <> "px")
    , ("transform", rot)
    , ("-ms-transform", rot)
    , ("-webkit-transform", rot)
  ] where
    stroke = 2
    ax :: Float
    ax = fromIntegral (fst a)
    ay = fromIntegral (snd a)
    bx = fromIntegral (fst b)
    by = fromIntegral (snd b)
    adjacent = abs (ax - bx)
    opposite = abs (ay - by)
    hypotenuse = sqrt (adjacent*adjacent + opposite*opposite)
    offsetx = abs $ (hypotenuse - adjacent) / 2
    x = round $ if bx <= ax then bx - offsetx else ax - offsetx
    offsety = (- opposite / 2) - stroke / 2
    y = round $ if by <= ay then ay + offsety else by + offsety
    --rotVal = atan2 opposite adjacent
    radians = atan (opposite / adjacent)
    rotVal
      | bx < ax && by < ay = radians
      | bx > ax && by > ay = radians
      | adjacent == 0 = pi / 2
      | otherwise = - radians
    rot = "rotate(" <> (T.pack $ (showFFloat (Just 3) rotVal "")) <> "rad)"

-- | draw line between two items (itemel1, itemel2)
-- thx https://github.com/kouky/line-css.js/blob/master/src/line-css.coffee
line :: (MonadWidget t m) => Pos -> Pos -> m ()
line a b = do
  let
    attrMap = M.fromList [
         styleToAttr (makeLineStyle a b)
        , ("class", "path")
      ]
  -- TODO triggers for highlighting
  --(modAttrEv, action) <- newTriggerEvent
  (e,_) <- elAttr' "div" attrMap blank
  return ()

-- | creates a widget for an item and returns its ItemMeta
itemBox :: (MonadWidget t m) => Item -> ItemAttr -> m (ItemMeta t)
itemBox item attr = do
  (modAttrEv, action) <- newTriggerEvent
  -- modAttrEv produces map of new attributes which we union with previous ones
  dynAttrs <- foldDyn M.union (toAttrMap attr) modAttrEv
  (e, _) <- elDynAttr' "div" dynAttrs $ text (unItemId (itemId item))
  return $ ItemMeta {
    im_item = item
    , im_attr = attr
    , im_trigger_action = action
    , im_click_event = domEvent Click e
    , im_mouseover_event = domEvent Mouseover e
    , im_mouseout_event = domEvent Mouseout e
  }

mapWithIndexM :: (Traversable t, Monad m) => (Int -> a -> m b) -> t a -> m (t b)
mapWithIndexM f ta = sequence $ snd (mapAccumL (\i a -> (i+1, f i a)) 0 ta)

mapKeysM :: (Monad m) => M.Map k a -> (k -> m b) -> m [b]
mapKeysM m f = M.foldrWithKey (\k _ acc -> (:) <$> f k <*> acc) (return []) m


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
      pos = (x * 100 + 100, tier * 100 + 100)
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
          Just endItemMeta -> line pos' (ia_pos . im_attr $ endItemMeta)
      return ()

    -- main widget
    potatoWidget :: forall t m. (MonadWidget t m) => m ()
    potatoWidget = mdo
      -- variable to track current selection (clickItemEv defined later on)
      currentSelection :: Behavior t (Maybe (ItemMeta t)) <- hold Nothing clickItemEv

      -- generate item boxes
      itemMetas' <- mapWithIndexM outerMap tiered
      let
        itemMetas :: [ItemMeta t]
        itemMetas = join itemMetas'
        itemMetaMap :: Map ItemId (ItemMeta t)
        itemMetaMap = M.fromList $ map (\im -> (itemId (im_item im), im)) itemMetas

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
            case maybeEndItemMeta of
              Nothing          -> blank
              Just endItemMeta -> selectRecursive imm icm endItemMeta
          im_trigger_action im ("class" =: "tech selected")

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

        selectAction :: Maybe (ItemMeta t) -> ItemMeta t -> IO ()
        selectAction prev im = do
          -- first unselect previous
          case prev of
            Nothing  -> return ()
            Just pim -> trace ("unselecting: " <> show (im_item pim)) $ unselectRecursive itemMetaMap allConnections pim
          -- then select current
          trace ("selecting: " <> show (im_item im)) $ selectRecursive itemMetaMap allConnections im

      -- set up on click triggers
      --forM_ itemMetas (\im -> performEvent ((\_ -> liftIO $ simpleTrigger im) <$> im_click_event im))
      clickItemEvs_ <- forM itemMetas (\im -> performEvent
        ((\prev -> liftIO $ selectAction prev im >> return im) <$> tag currentSelection (im_click_event im)))

      let
        -- create an event that fires after any of the actions above is performed
        clickItemEv :: Event t (Maybe (ItemMeta t)) = Just <$> leftmost clickItemEvs_

      -- draw lines
      forM_ itemMetas (lineMap itemMetaMap allConnections)

      return ()


  -- actually render stuff now
  mainWidgetWithCss css $ do potatoWidget
  --mainWidget $ do

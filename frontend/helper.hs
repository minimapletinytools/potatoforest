module Helper (
  Pos
  , offsetPos
  , posToStyle
  , Style
  , styleToAttr
  , makeLineStyle
  , mapWithIndexM
  , mapKeysM
) where

import qualified Data.Map  as M
import           Numeric   (showFFloat)
import           Prelude   (atan2)
import           Relude

import qualified Data.Text as T

type Pos = (Int, Int)

offsetPos :: Pos -> Pos -> Pos
offsetPos (a,b) (c,d) = (a+c, b+d)

posToStyle :: Pos -> Style
posToStyle (a,b) = M.fromList [("left", show a <> "px"),("top", show b <> "px")]

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

mapWithIndexM :: (Traversable t, Monad m) => (Int -> a -> m b) -> t a -> m (t b)
mapWithIndexM f ta = sequence $ snd (mapAccumL (\i a -> (i+1, f i a)) 0 ta)

mapKeysM :: (Monad m) => M.Map k a -> (k -> m b) -> m [b]
mapKeysM m f = M.foldrWithKey (\k _ acc -> (:) <$> f k <*> acc) (return []) m

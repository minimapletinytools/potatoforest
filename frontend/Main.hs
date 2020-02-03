

module Main where

import           Potato.Forest.Methods
import           Reflex.Dom
import           Relude

import qualified Data.Map              as M
import           Data.Text             (pack, unpack)

potatomain :: IO ()
potatomain = mainWidget $ el "div" $ do
  el "ul" $ do
    el "li" $ text helloPotato
    el "li" $ do
      t <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      dynText $ _inputElement_value t


main :: IO ()
main = mainWidget $ el "div" $ do
  nx <- numberInput
  d <- dropdown Times (constDyn ops) def
  ny <- numberInput
  let
    values = zipDynWith (,) nx ny
    result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
    resultText = fmap (pack . show) result
  text " = "
  dynText resultText

numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
numberInput = do
  n <- inputElement $ def
    & inputElementConfig_initialValue .~ "0"
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
  return . fmap (readMaybe . unpack) $ _inputElement_value n

data MyOp = Plus | Minus | Times | Divide deriving (Eq, Ord)

ops :: M.Map MyOp Text
ops = M.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

runOp :: Fractional a => MyOp -> a -> a -> a
runOp s = case s of
           Plus   -> (+)
           Minus  -> (-)
           Times  -> (*)
           Divide -> (/)

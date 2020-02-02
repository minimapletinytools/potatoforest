module Main (main) where

import           Relude                hiding (readFile)

import           Control.Applicative   hiding (some)
import           Data.Text.IO
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

import Potato.Forest.Types as P
import           Potato.Forest.Parser


testItem :: P.Item
testItem = Item {
  itemId = ItemId "test_item"
  , title = "this item is for testing"
  , desc = "don't use me!"
  , limit = Nothing
  , tier = Just 0
}

test_Item_Eq :: Spec
test_Item_Eq = describe "Eq" $ do
  it "equates two identical items" $
    (testItem == testItem) `shouldBe` True
  it "equates two different items with same id" $
    (testItem == testItem { tier = Just 1 }) `shouldBe` True
  it "does not equate two items with different id" $
    (testItem == testItem { itemId = ItemId "test_item'" }) `shouldBe` False

test_runForestBlocksParser :: Text -> Spec
test_runForestBlocksParser testInput = describe "runForestBlocksParser" $ do
  it "does not crash on test input" $ do
    runForestBlocksParser testInput `shouldSatisfy` \case
      Left x -> trace (errorBundlePretty x) $ False
      Right x -> True

main :: IO ()
main = do
  testInput <- readFile "../examples/testing.spec"
  hspec $ do
    describe "types" $ do
      describe "Item" $ do
        test_Item_Eq
    describe "parser" $ do
      test_runForestBlocksParser testInput

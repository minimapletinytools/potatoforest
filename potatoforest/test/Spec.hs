module Main (main) where

import           Relude                hiding (readFile)

import           Control.Applicative   hiding (some)
import           Data.Text.IO
import           Data.Void
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Char

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


main :: IO ()
main = do
  testInput <- readFile "../examples/testing.spec"
  print $ runForestParser' forestBlocksParser testInput
  hspec $ do
    describe "types" $ do
      describe "Item" $ do
        test_Item_Eq
    describe "parser" $ do
      it "does not crash" $
        (\s -> runForestParser' forestBlocksParser s) `shouldSucceedOn` testInput

module Main (main) where

import           Relude               hiding (readFile)

import           Data.Text.IO
import qualified Data.Text.Lazy       as LT
import           Test.Hspec
--import           Test.Hspec.Megaparsec
import           Test.HUnit.Lang
import           Text.Megaparsec

import           Potato.Forest.Parser
import           Potato.Forest.Types  as P
import           Text.Pretty.Simple   (pShow)





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

parseFile :: String -> IO (Maybe ForestBlocks)
parseFile filename = do
  input <- readFile filename
  case runForestBlocksParser input of
    Left x  -> trace (errorBundlePretty x) $ return Nothing
    Right x -> return $ Just x

test_runForestBlocksParser :: String -> Spec
test_runForestBlocksParser filename = describe "runForestBlocksParser" $ do
  it ("does not crash on test input " ++ filename) $ do
    r <- parseFile filename
    r `shouldSatisfy` \case
      Nothing -> False
      otherwise -> True

test_generateTieredItems :: Spec
test_generateTieredItems = describe "generateTieredItems" $ do
  it ("handles simple case ok") $ do
    r <- parseFile "../examples/testing2.spec"
    case r of
      Nothing -> assertFailure "could not read file"
      Just blocks -> do
        let
          itemSet = knownItems blocks
          recipeSet = knownRecipes blocks
        generateTieredItems itemSet recipeSet `shouldSatisfy` (\x -> trace (LT.unpack (pShow x)) $ True)


main :: IO ()
main = do
  hspec $ do
    describe "types" $ do
      describe "Item" $ do
        test_Item_Eq
    describe "parser" $ do
      test_runForestBlocksParser "../examples/testing1.spec"
    describe "unlabeled" $ do
      test_generateTieredItems

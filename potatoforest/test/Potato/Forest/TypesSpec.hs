module Potato.Forest.TypesSpec (
  spec
) where
import           Relude
import           Test.Hspec
import           Potato.Forest.Types


testItem :: Item
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

spec :: Spec
spec = do
  describe "Types" $ do
    describe "Item" $ do
      test_Item_Eq

module Potato.Forest.MethodsSpec (
  spec
) where

import Potato.Forest.ParserSpec hiding (spec)

import qualified Data.Map                          as M
import qualified Data.Set                          as S
import           Data.Default

import           Test.HUnit.Lang
import           Relude
import           Test.Hspec
import           Potato.Forest.Methods
import           Potato.Forest.Methods2
import           Potato.Forest.Parser
import           Potato.Forest.Types

-- | makes test items with prefix
makeItemsForTest :: Text -> Int -> [Item]
makeItemsForTest prefix n = map (\x -> def {itemId = ItemId $ prefix <> show x}) [0..(n-1)]


test_generateTieredItems :: Spec
test_generateTieredItems = describe "generateTieredItems" $ do
  let
    --withFile :: Text -> (ForestBlocks -> Spec) -> Spec <- figure out what last type should be
    withFile filename f = do
      r <- parseFile filename
      case r of
        Nothing     -> assertFailure "could not read file"
        Just blocks -> f blocks
  it ("handles simple case ok") $ do
    withFile "tier_simple.spec" $ \blocks ->
      generateTieredItems (knownItems blocks) (knownRecipes blocks) `shouldSatisfy` flip deepseq True
        --generateTieredItems itemSet recipeSet `shouldSatisfy` (\x -> trace (LT.unpack (pShow x)) $ True)
  it ("handles circular case") $ do
    withFile "tier_circular.spec" $ \blocks ->
      generateTieredItems (knownItems blocks) (knownRecipes blocks) `shouldSatisfy` flip deepseq True

-- Method2 stuff
test_clearInTuple :: Spec
test_clearInTuple = describe "clearInTuple" $ do
  let
    self = def { itemId = "self" }
    child = def { itemId = "child" }
    parent = def { itemId = "parent" }
    other = makeItemsForTest "other" 0
    adjs = M.fromList [
      (self, ([parent],[child]))
      , (parent, (other, self:other))
      , (child, (self:other, other))
      ]
    adjs1 = M.fromList [
      (self, ([parent], [child]))
      , (parent, (other, self:other))
      , (child, (other, other))
      ]
    adjs2 = M.fromList [
      (self, ([parent], [child]))
      , (parent, (other, other))
      , (child, (self:other, other))
      ]
  -- note this test assumes implementation dependendent order preserving properties of clearInChildren and clearInParents
  it "clearItemFromChildren behaves as expected" $ do
    clearItemFromChildren self adjs `shouldBe` adjs1
  it "clearItemFromParents behaves as expected" $ do
    clearItemFromParents self adjs `shouldBe` adjs2


test_buildAdjs :: Spec
test_buildAdjs = describe "buildAdjs" $ do
  let
    a = def { itemId = "A" }
    b = def { itemId = "B" }
    c = def { itemId = "C" }
    -- we don't bother making proper recipes, they aren't used
    s :: Item -> ItemConnections
    s x = M.fromList [(x, S.empty)]
    -- a -> b -> c -> a
    icm = M.fromList [(a, s b), (b, s c), (c, s a)]
    expected = M.fromList [(a, ([c], [b])), (b, ([a], [c])), (c, ([b], [a]))]
  it "behaves as expected in basic test case" $ do
    buildAdjs icm a M.empty `shouldBe` expected

spec :: Spec
spec = do
  describe "Methods" $ do
    test_generateTieredItems
  describe "Methods2" $ do
    test_clearInTuple

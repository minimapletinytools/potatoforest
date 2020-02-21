module Potato.Forest.MethodsSpec (
  spec
) where

import           Potato.Forest.ParserSpec hiding (spec)

import           Data.Default
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import qualified Data.Text.Lazy           as LT
import           Text.Pretty.Simple

import           Potato.Forest.Methods
import           Potato.Forest.Methods2
import           Potato.Forest.Parser
import           Potato.Forest.Types
import           Relude
import           Test.Hspec
import           Test.HUnit.Lang

trace_pShow :: (Show a) => a -> b -> b
trace_pShow x = trace (LT.unpack (pShow x))

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
        --Just blocks -> trace_pShow blocks $ f blocks
        Just blocks -> f blocks
  it ("handles simple case ok") $ do
    withFile "tier_simple.spec" $ \blocks ->
      newGenerateTieredItems (knownItems blocks) (knownRecipes blocks) `shouldSatisfy` flip deepseq True
        --generateTieredItems itemSet recipeSet `shouldSatisfy` (\x -> trace (LT.unpack (pShow x)) $ True)
  it ("handles simple circular case") $ do
    withFile "tier_circular.spec" $ \blocks ->
      newGenerateTieredItems (knownItems blocks) (knownRecipes blocks) `shouldSatisfy` flip deepseq True
  it ("handles hard circular case") $ do
    withFile "tier_circular_hard.spec" $ \blocks ->
      newGenerateTieredItems (knownItems blocks) (knownRecipes blocks) `shouldSatisfy` (\x -> trace (LT.unpack (pShow x)) $ True)

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

test_evalTierFn' :: Spec
test_evalTierFn' = describe "evalTierFn'" $ do
  let
    empty = []
    allNothing = [Nothing, Nothing, Nothing]
    onlyOne = [Just 1]
    onlyFive = [Just 5]
    someNothing = onlyOne <> allNothing
  it "always tier 0 when children are empty" $ do
    evalTierFn' (empty, empty) `shouldBe` 0
    evalTierFn' (allNothing, empty) `shouldBe` 0
    evalTierFn' (onlyFive, empty) `shouldBe` 0
  it "having no parents is same as having some parent that is nothing" $ do
    evalTierFn' ([], allNothing) `shouldBe` evalTierFn' (allNothing, allNothing)
    evalTierFn' ([], allNothing) `shouldBe` evalTierFn' (someNothing, allNothing)
  it "if all children are nothing and some parent is nothing, we are in a base loop" $ do
    evalTierFn' (allNothing, allNothing) `shouldBe` 0
    evalTierFn' (someNothing, allNothing) `shouldBe` 0
  it "child tier + 1 when parents are all nothing" $ do
    evalTierFn' (allNothing, onlyOne) `shouldBe` 2
    evalTierFn' (allNothing, onlyFive) `shouldBe` 6
  it "is bounded by parent tier appropriately" $ do
    evalTierFn' (onlyOne, onlyOne) `shouldBe` 1
    evalTierFn' (onlyFive, onlyOne) `shouldBe` 2
  it "parent tier can not push tier lower than max child tier" $ do
    evalTierFn' (onlyOne, onlyFive) `shouldBe` 5


spec :: Spec
spec = do
  describe "Methods" $ do
    test_generateTieredItems
  describe "Methods2" $ do
    test_buildAdjs
    test_clearInTuple
    test_evalTierFn'

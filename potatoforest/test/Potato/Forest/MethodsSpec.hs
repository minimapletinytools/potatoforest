module Potato.Forest.MethodsSpec (
  spec
) where

import Potato.Forest.ParserSpec hiding (spec)

import           Test.HUnit.Lang
import           Relude
import           Test.Hspec
import           Potato.Forest.Methods
import           Potato.Forest.Parser
import           Potato.Forest.Types


-- TODO use deepseq to force pls...
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
      generateTieredItems (knownItems blocks) (knownRecipes blocks) `shouldSatisfy` (\x -> length x < 100)
        --generateTieredItems itemSet recipeSet `shouldSatisfy` (\x -> trace (LT.unpack (pShow x)) $ True)
  it ("handles circular case") $ do
    withFile "tier_circular.spec" $ \blocks ->
      generateTieredItems (knownItems blocks) (knownRecipes blocks) `shouldSatisfy` (\x -> length x < 100)


spec :: Spec
spec = do
  describe "Methods" $ do
    test_generateTieredItems

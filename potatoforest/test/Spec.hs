module Main (main) where

import           Relude                hiding (readFile)

import           Control.Applicative   hiding (some)
import           Data.Text.IO
import           Data.Void
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Potato.Forest.Parser



main :: IO ()
main = do
  testInput <- readFile "../examples/testing.spec"
  print $ runForestParser' forestBlocksParser testInput
  hspec $
    describe "parser" $ do
      it "does not crash" $
        (\s -> runForestParser' forestBlocksParser s) `shouldSucceedOn` testInput

module Main (main) where

import           Control.Applicative   hiding (some)
import           Data.Text             (Text)
import           Data.Void
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Char


main :: IO ()
main = return ()

-- TODO
{-
main :: IO ()
main = hspec $
  describe "parser" $ do
    it "returns correct result" $
      parse myParser "" "aaa" `shouldParse` "aaa"
    it "result of parsing satisfies what it should" $
      parse myParser "" "aaaa" `parseSatisfies` ((== 4) . length)
-}

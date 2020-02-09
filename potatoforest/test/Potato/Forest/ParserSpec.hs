module Potato.Forest.ParserSpec (
  spec
  , parseFile
) where


import           Relude                hiding (readFile)

import           Data.Text.IO
import qualified Data.Text.Lazy        as LT
import           Text.Pretty.Simple    (pShow)

--import           Test.Hspec.Megaparsec
import           Test.Hspec
import           Test.HUnit.Lang
import           Text.Megaparsec

import           Potato.Forest.Parser
import           Potato.Forest.Types

parseFile :: String -> IO (Maybe ForestBlocks)
parseFile filename = do
  input <- readFile filename
  case runForestBlocksParser input of
    Left x  -> trace (errorBundlePretty x) $ return Nothing
    Right x -> return $ Just x
    --Right x -> trace (LT.unpack (pShow x)) $ return $ Just x

test_identifier :: Spec
test_identifier = describe "parser: identifier" $ do
  it "rejects all caps identifiers" $
    runForestParser' identifier "ALLCAPS" `shouldSatisfy` isLeft
  it "rejects all reserved prefix" $
    runForestParser' identifier "__________nogood" `shouldSatisfy` isLeft

test_parseTextBlob :: Spec
test_parseTextBlob = describe "parser: parseTextBlob" $ do
  let
    end = "\nDESC stuff here never gets parsed"
    tb1 = " some text\n\n\nhi\r\n\n\r\nbye\nok\nno"
    tb2 = "keywords in middle of sentence DESC TITLE ITEM is ok"
    tb3 = "space after newline is ok"
    tb3End = "\n        \t  \t   DESC"
  it "handles many new different new lines" $
    runForestParser' parseTextBlob (tb1 <> end) `shouldBe` Right tb1
  it "handles allows keywords in middle of blob" $
    runForestParser' parseTextBlob (tb2 <> end) `shouldBe` Right tb2
  it "handles spaces before keyword" $
    runForestParser' parseTextBlob (tb3 <> tb3End) `shouldBe` Right tb3

test_parseTags :: Spec
test_parseTags = describe "parser: parseTags" $ do
  let
    end = "\n stuff here never \n gets parsed"
  it "handles many spaces" $
    runForestParser' parseTags ("t1 t2   \t t3 t4" <> end) `shouldBe` Right ["t1","t2","t3","t4"]
  it "fails on identifiers" $
    runForestParser' parseTags ("t1 DESC t2" <> end) `shouldSatisfy` isLeft

test_parseFilename :: Spec
test_parseFilename = describe "parser: parseFilename" $ do
  it "parses a filename" $
    runForestParser' parseFilename ("not.a-virus.exe\n not parsed") `shouldBe` Right "not.a-virus.exe"

test_parsePhantom :: Spec
test_parsePhantom = describe "parser: parsePhantom" $ do
  it "parses a omit" $
    runForestParser' parsePhantom ("omit") `shouldBe` Right Omit
  it "parses pass" $
    runForestParser' parsePhantom ("pass") `shouldBe` Right Pass
  it "fails otherwise" $
    runForestParser' parsePhantom ("OmIt") `shouldSatisfy` isLeft


test_runForestBlocksParser :: String -> Spec
test_runForestBlocksParser filename = describe "runForestBlocksParser" $ do
  it ("does not crash on test input " ++ filename) $ do
    r <- parseFile filename
    r `shouldSatisfy` \case
      Nothing -> False
      otherwise -> True
      
spec :: Spec
spec = do
  describe "Parser" $ do
    test_runForestBlocksParser "testing1.spec"
    --test_runForestBlocksParser "../examples/sc1.spec"
    test_identifier
    test_parseTextBlob
    test_parseTags
    test_parseFilename

module Test.Hspec.GoldenSpec (spec) where

import           Control.Monad          (void)

import           Test.Hspec
import qualified Test.Hspec.Core.Runner as H
import qualified Test.Hspec.Core.Spec   as H
import           Test.Hspec.Golden

import           System.Directory
import           System.IO.Silently

{-# ANN module "HLint: ignore Reduce duplication" #-}

fixtureContent, fixtureTestName, fixtureUpdatedContent :: String
fixtureUpdatedContent = "different text"
fixtureContent = "simple text"
fixtureTestName = "id"

goldenTestDir, goldenFilePath, actualFilePath :: FilePath
goldenTestDir = ".golden" ++ "/" ++ "id"
goldenFilePath = goldenTestDir ++ "/" ++ "golden"
actualFilePath = goldenTestDir ++ "/" ++ "actual"

fixtureTest :: String -> H.Spec
fixtureTest content =
  H.describe "id" $
    H.it "should work" $
      defaultGolden fixtureTestName content

removeFixtures :: IO ()
removeFixtures = removeDirectoryRecursive goldenTestDir

runSpec :: H.Spec -> IO [String]
runSpec = captureLines . H.hspecResult

captureLines :: IO a -> IO [String]
captureLines = fmap lines . capture_

spec :: Spec
spec =
  describe "Golden" $ after_ removeFixtures $ do
    context "when the test is executed for the first time" $ do
      it "should create a `golden` file" $ do
         void $ runSpec $ fixtureTest fixtureContent
         goldenFileContent <- readFile goldenFilePath
         goldenFileContent `shouldBe` fixtureContent

      it "should create a `actual` file" $ do
         void $ runSpec $ fixtureTest fixtureContent
         actualFileContent <- readFile goldenFilePath
         actualFileContent `shouldBe` fixtureContent

    context "when the output is updated" $
      context "when the test is executed a second time" $ do
        it "should create the `actual` output file" $ do
           void $ runSpec $ fixtureTest fixtureContent
           void $ runSpec $ fixtureTest fixtureUpdatedContent
           actualFileContent <- readFile actualFilePath
           actualFileContent `shouldBe` fixtureUpdatedContent

        it "shouldn't overide the `golden` file" $ do
           void $ runSpec $ fixtureTest fixtureContent
           void $ runSpec $ fixtureTest fixtureUpdatedContent
           goldenFileContent <- readFile goldenFilePath
           goldenFileContent `shouldBe` fixtureContent


    context "when the output is not updated" $
      context "when the test is executed a second time" $
        it "shouldn't change the `golden` file content" $ do
           void $ runSpec $ fixtureTest fixtureContent
           void $ runSpec $ fixtureTest fixtureContent
           goldenFileContent <- readFile goldenFilePath
           goldenFileContent `shouldBe` fixtureContent

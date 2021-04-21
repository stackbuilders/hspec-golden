{-|
Module      : Test.Hspec.Golden
Description : Golden tests for Hspec
Copyright   : Stack Builders (c), 2019-2020
License     : MIT
Maintainer  : cmotoche@stackbuilders.com
Stability   : experimental
Portability : portable

Golden tests store the expected output in a separated file. Each time a golden test
is executed the output of the subject under test (SUT) is compared with the
expected output. If the output of the SUT changes then the test will fail until
the expected output is updated. We expose 'defaultGolden' for output of
type @String@. If your SUT has a different output, you can use 'Golden'.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Test.Hspec.Golden
  ( Golden(..)
  , defaultGolden
  )
  where

import           Data.IORef
import           System.Directory     (createDirectoryIfMissing, doesFileExist)
import           System.FilePath      (takeDirectory, (</>))
import           Test.Hspec.Core.Spec (Example (..), FailureReason (..),
                                       Result (..), ResultStatus (..))


-- | Golden tests parameters
--
-- @
-- import           Data.Text (Text)
-- import qualified Data.Text.IO as T
--
-- goldenText :: String -> Text -> Golden Text
-- goldenText name actualOutput =
--   Golden {
--     output = actualOutput,
--     encodePretty = prettyText,
--     writeToFile = T.writeFile,
--     readFromFile = T.readFile,
--     goldenFile = ".specific-golden-dir" </> name </> "golden",
--     actualFile = Just (".specific-golden-dir" </> name </> "actual"),
--     failFirstTime = False
--   }
--
-- describe "myTextFunc" $
--   it "generates the right output with the right params" $
--     goldenText "myTextFunc" (myTextFunc params)
-- @

data Golden str =
  Golden {
    output        :: str, -- ^ Output
    encodePretty  :: str -> String, -- ^ Makes the comparison pretty when the test fails
    writeToFile   :: FilePath -> str -> IO (), -- ^ How to write into the golden file the file
    readFromFile  :: FilePath -> IO str, -- ^ How to read the file,
    goldenFile    :: FilePath, -- ^ Where to read/write the golden file for this test.
    actualFile    :: Maybe FilePath, -- ^ Where to save the actual file for this test. If it is @Nothing@ then no file is written.
    failFirstTime :: Bool -- ^ Whether to record a failure the first time this test is run
  }

instance Eq str => Example (Golden str) where
  type Arg (Golden str) = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Eq str => Example (arg -> Golden str) where
  type Arg (arg -> Golden str) = arg
  evaluateExample golden _ action _ = do
    ref <- newIORef (Result "" Success)
    action $ \arg -> do
      r <- runGolden (golden arg)
      writeIORef ref (fromGoldenResult r)
    readIORef ref

-- | Transform a GoldenResult into a Result from Hspec

fromGoldenResult :: GoldenResult -> Result
fromGoldenResult SameOutput             = Result "Golden and Actual output hasn't changed" Success
fromGoldenResult FirstExecutionSucceed  = Result "First time execution. Golden file created." Success
fromGoldenResult FirstExecutionFail =
  Result "First time execution. Golden file created."
         (Failure Nothing (Reason "failFirstTime is set to True"))
fromGoldenResult (MissmatchOutput expected actual) =
  Result "Files golden and actual not match"
         (Failure Nothing (ExpectedButGot Nothing expected actual))

-- | An example of Golden tests which output is 'String'
--
-- @
--  describe "html" $ do
--    context "given a valid generated html" $
--      it "generates html" $
--        defaultGolden "html" someHtml
-- @

defaultGolden :: String -> String -> Golden String
defaultGolden name output_ =
  Golden {
    output = output_,
    encodePretty = show,
    writeToFile = writeFile,
    readFromFile = readFile,
    goldenFile = ".golden" </> name </> "golden",
    actualFile = Just (".golden" </> name </> "actual"),
    failFirstTime = False
  }

-- | Possible results from a golden test execution

data GoldenResult =
   MissmatchOutput String String
   | SameOutput
   | FirstExecutionSucceed
   | FirstExecutionFail

-- | Runs a Golden test.

runGolden :: Eq str => Golden str -> IO GoldenResult
runGolden Golden{..} =
  let goldenTestDir = takeDirectory goldenFile
   in do
     createDirectoryIfMissing True goldenTestDir
     goldenFileExist <- doesFileExist goldenFile

     case actualFile of
       Nothing -> return ()
       Just actual -> do
           -- It is recommended to always write the actual file, this way,
           -- hgold will always upgrade based on the latest run
           let actualDir = takeDirectory actual
           createDirectoryIfMissing True actualDir
           writeToFile actual output

     if not goldenFileExist
       then do
           writeToFile goldenFile output
           return $ if failFirstTime
               then FirstExecutionFail
               else FirstExecutionSucceed
       else do
          contentGolden <- readFromFile goldenFile

          if contentGolden == output
             then return SameOutput
             else return $ MissmatchOutput (encodePretty contentGolden) (encodePretty output)

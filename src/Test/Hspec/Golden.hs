{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Hspec.Golden
  ( Golden(..)
  , GoldenResult(..)
  , defaultGolden
  , runGolden
  )
  where

import           Data.IORef
import           Test.Hspec.Core.Spec ( Example(..)
                                      , Result(..)
                                      , ResultStatus(..)
                                      , FailureReason(..)
                                      )
import           System.Directory ( createDirectoryIfMissing
                                  , doesFileExist
                                  )


-- | Golden tests parameters

data Golden str =
  Golden {
    output :: str, -- ^ Lazy bytestring output
    writeToFile :: FilePath -> str -> IO (), -- ^ How to write into the golden file the file
    readFromFile :: FilePath -> IO str, -- ^ How to read the file,
    testName :: String, -- ^ Test name (make sure it's unique otherwise it could be override)
    directory :: FilePath -- ^ Directory where you write your tests
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
fromGoldenResult FirstExecution  = Result "First time execution. Golden file created." Success
fromGoldenResult SameOutput      = Result "Golden and Actual output hasn't changed" Success
fromGoldenResult MissmatchOutput =
  Result "Files golden and actual missmatch"
         (Failure Nothing (Reason "Files golden and actual missmatch"))

-- | An example of Golden tests which output is 'String'

defaultGolden :: String -> String -> Golden String
defaultGolden name output_ =
  Golden {
    output = output_,
    testName = name,
    writeToFile = writeFile,
    readFromFile = readFile,
    directory = ".golden"
  }

-- | Possible results from a golden test execution

data GoldenResult =
   MissmatchOutput
   | SameOutput
   | FirstExecution

-- | Runs a Golden test.

runGolden :: Eq str => Golden str -> IO GoldenResult
runGolden Golden{..} =
  let goldenTestDir = directory ++ "/" ++ testName
      goldenFilePath = goldenTestDir ++ "/" ++ "golden"
      actualFilePath = goldenTestDir ++ "/" ++ "actual"
   in do
     createDirectoryIfMissing True goldenTestDir
     goldenFileExist <- doesFileExist goldenFilePath
     actualFileExist <- doesFileExist actualFilePath
     case (goldenFileExist, actualFileExist) of
       (False, _)    -> writeToFile goldenFilePath output
                             >> return FirstExecution
       (True, False) -> do
          contentGolden <- readFromFile goldenFilePath
          if contentGolden == output
             then return SameOutput
             else writeToFile actualFilePath output
                    >> return MissmatchOutput

       (True, True) -> do
          contentGolden <- readFromFile goldenFilePath
          contentActual <- readFromFile actualFilePath
          if contentGolden == contentActual
             then return SameOutput
             else writeToFile actualFilePath output
                    >> return MissmatchOutput

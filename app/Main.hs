{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad (forM_, when)
import           Data.Version (showVersion)
import           Paths_hspec_golden (version)
import           SimpleGetOpt
import           System.Directory (doesFileExist, listDirectory, renameFile)
import qualified Test.Hspec.Golden as G

-- CLI Parameters
data Params =
  Params
    { version_  :: Bool
    , updateDir :: FilePath
    } deriving Show

defaultDirGoldenTest :: FilePath
defaultDirGoldenTest = G.directory (G.defaultGolden "" "")

opts :: OptSpec Params
opts =
  OptSpec
    { progDefaults = Params False defaultDirGoldenTest
    , progOptions =
      [ Option ['u'] ["update"] "Replaces `golden` file with `actual`"
        $ OptArg "DIR" $ \maybeDir ->
          case maybeDir of
            Nothing -> \s -> Right s { updateDir = defaultDirGoldenTest }
            Just dir -> \s -> Right s { updateDir = dir }
      , Option ['v'] ["version"] "Displays the version of hgold"
        $ NoArg $ \s -> Right s { version_ = True }
      ]
    , progParamDocs = []
    , progParams = const Right
    }

-- Update files
updateGolden :: FilePath -> IO ()
updateGolden dir = do
  goldenTests <- listDirectory dir
  forM_ goldenTests (mvActualToGolden dir)

mvActualToGolden :: FilePath -> FilePath -> IO ()
mvActualToGolden goldenDir testName =
  let actualFilePath = goldenDir ++ "/" ++ testName ++ "/" ++ "actual"
      goldenFilePath = goldenDir ++ "/" ++ testName ++ "/" ++ "golden"
   in do
     actualFileExist <- doesFileExist actualFilePath
     when actualFileExist (renameFile actualFilePath goldenFilePath)

-- MAIN

main :: IO ()
main = do
  Params {..} <- getOpts opts
  if version_
    then putStrLn $ showVersion version
    else updateGolden updateDir

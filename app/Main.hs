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
    { version_     :: Bool
    , showHelp     :: Bool
    , updateDir    :: FilePath
    , shouldUpdate :: Bool
    } deriving Show

defaultDirGoldenTest :: FilePath
defaultDirGoldenTest = G.directory (G.defaultGolden "" "")

opts :: OptSpec Params
opts =
  OptSpec
    { progDefaults = Params False True defaultDirGoldenTest False
    , progOptions =
      [ Option ['u'] ["update"] "Replaces `golden` files with `actual` files"
        $ OptArg "DIR" $ \maybeDir ->
          case maybeDir of
            Nothing -> \s ->
              Right s
                { shouldUpdate = True
                , updateDir = defaultDirGoldenTest
                , showHelp = False
                }
            Just dir -> \s ->
              Right s
                { shouldUpdate = True
                , updateDir = dir
                , showHelp = False
                }
      , Option ['v'] ["version"] "Displays the version of hgold"
        $ NoArg $ \s -> Right s { version_ = True, showHelp = False }
      , Option ['h'] ["help"] "Displays help information"
        $ NoArg $ \s -> Right s
      ]
    , progParamDocs = [("DIR", "The testing directory where you're dumping your results (default: .golden/)")]
    , progParams = const Right
    }

-- Update files
updateGolden :: FilePath -> IO ()
updateGolden dir = do
  putStrLn "Replacing golden with actual..."
  goldenTests <- listDirectory dir
  forM_ goldenTests (mvActualToGolden dir)
  putStrLn "Finish..."

mvActualToGolden :: FilePath -> FilePath -> IO ()
mvActualToGolden goldenDir testName =
  let actualFilePath = goldenDir ++ "/" ++ testName ++ "/" ++ "actual"
      goldenFilePath = goldenDir ++ "/" ++ testName ++ "/" ++ "golden"
   in do
     actualFileExist <- doesFileExist actualFilePath
     when actualFileExist (do
       putStrLn $ "  Replacing file: " ++ goldenFilePath ++ " with: " ++ actualFilePath
       renameFile actualFilePath goldenFilePath)

-- MAIN

main :: IO ()
main = do
  Params {..} <- getOpts opts
  when showHelp (dumpUsage opts)
  when version_ (putStrLn $ showVersion version)
  when shouldUpdate (updateGolden updateDir)

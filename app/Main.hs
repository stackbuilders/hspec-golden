{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad         (forM_, when)
import           Data.Version          (showVersion)
import           Paths_hspec_golden    (version)
import           System.Console.Docopt
import           System.Directory      (doesFileExist, listDirectory,
                                        renameFile)
import           System.Environment    (getArgs)
import qualified Test.Hspec.Golden     as G

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

patterns :: Docopt
patterns = [docopt|
Usage:
  hgold [-u=<file>]

Options:
  -u=<file>  specify output file
|]

getArgOrExit = getArgOrExitWith patterns

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
  args <- parseArgsOrExit patterns =<< getArgs
  when (args `isPresent` command "update") $
    let goldenDir = (args `getArgWithDefault` defaultDirGoldenTest) (argument "golden_dir")
     in print goldenDir
--  when showHelp (dumpUsage opts)
--  when version_ (putStrLn $ showVersion version)
--  when shouldUpdate (updateGolden updateDir)

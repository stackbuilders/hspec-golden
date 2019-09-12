module Main where

import           Control.Monad         (forM_, when)
import           Data.Version          (showVersion)
import           Paths_hspec_golden    (version)
import           Options.Applicative
import           Data.Monoid ((<>))
import           System.Directory      (doesFileExist, listDirectory,
                                        renameFile)
import           System.Environment    (getArgs)
import qualified Test.Hspec.Golden     as G

defaultDirGoldenTest :: FilePath
defaultDirGoldenTest = G.directory (G.defaultGolden "" "")

-- CLI Params

data Params =
    Params
      { updateDir    :: FilePath 
   --   , shouldUpdate :: Bool
      } deriving Show
  
params :: Parser Params
params = Params 
      <$> strOption
          (long "update"
          <> short 'u'
          <> metavar "[DIR]"
          <> value defaultDirGoldenTest
          <> showDefault
          <> help "The testing directory where you're dumping your results.")

versionOpt :: Parser (a->a)
versionOpt = infoOption (showVersion version) 
              (long "version"
              <> short 'v'  
              <> help "Show version")
            

--Update Files
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


-- Main

main :: IO ()
main = updateGolden =<< execParser opts
  where

  opts = info ((updateDir <$> params) <**> versionOpt <**> helper)
        ( fullDesc <> header "Update your golden files" )
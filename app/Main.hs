module Main where

import           Control.Monad       (forM_, when)
import           Data.Monoid         ((<>))
import           Data.Version        (showVersion)
import           GHC.IO              (catch)
import           Options.Applicative
import           Paths_hspec_golden  (version)
import           System.Console.ANSI
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      listDirectory, renameFile)
import qualified Test.Hspec.Golden   as G
import System.FilePath((</>))
import Data.List (groupBy, isInfixOf)

defaultDirGoldenTest :: FilePath
defaultDirGoldenTest = ".golden"

-- CLI Params

newtype Params = Params { updateDir :: FilePath } deriving Show

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

withColor :: Color -> IO () -> IO ()
withColor color action = do
  setSGR [SetColor Foreground Dull color]
  action
  setSGR [Reset]

success, warning, failure :: IO () -> IO ()
success = withColor Green
warning = withColor Yellow
failure = withColor Red

-- Update golden files in the given directory
updateGolden :: FilePath -> IO ()
updateGolden dir = do
  putStrLn "Replacing golden with actual:"
  udpateFilesInDir dir
  success $ putStrLn "Finished!"
 where
  getBaseFileName filename = takeWhile (/= '-') filename 
  udpateFilesInDir dir = do
    entries <- listDirectory dir

    let groupedEntries = 
          [ (file1, file2)
            | [file1, file2] <- 
                groupBy 
                  (\file1 file2 -> 
                    getBaseFileName file1 == getBaseFileName file2
                  ) entries,
                  "-actual" `isInfixOf` file1,
                  "-golden" `isInfixOf` file2
          ]

    dirExists <- doesDirectoryExist dir
    if not dirExists
    then warning $ putStrLn (dir <> " does not exist")
    else forM_ groupedEntries $ \(actual, golden) ->
      mvActualToGolden (dir </> actual) (dir </> golden)

mvActualToGolden :: FilePath -> FilePath -> IO ()
mvActualToGolden actualFilePath goldenFilePath = do
  actualFileExist <- doesFileExist actualFilePath
  when actualFileExist $ do
    putStr "  Replacing file: "
    warning $ putStr goldenFilePath
    putStr " with: "
    success $ putStrLn actualFilePath
    renameFile actualFilePath goldenFilePath `catch` handleErr
     where
       handleErr :: IOError -> IO ()
       handleErr e =
         failure $ putStr $ "Warning: Could not replace file due to error: " ++ show e

-- Main

main :: IO ()
main = updateGolden =<< execParser opts
  where

  opts = info ((updateDir <$> params) <**> versionOpt <**> helper)
        ( fullDesc <> header "Update your golden files" )

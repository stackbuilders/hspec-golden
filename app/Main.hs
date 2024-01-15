module Main where

import           System.Console.ANSI
import           Control.Monad         (forM_, when)
import           Data.Version          (showVersion)
import           Paths_hspec_golden    (version)
import           Options.Applicative
import           Data.Monoid ((<>))
import           GHC.IO (catch)
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        listDirectory, renameFile)
import qualified Test.Hspec.Golden     as G

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
  go dir
  success $ putStrLn "Finished!"
 where
  go dir = do
    entries <- listDirectory dir
    forM_ entries $ \entry -> do
      let entryInDir = dir ++ "/" ++ entry
      isDir <- doesDirectoryExist entryInDir
      when isDir $ do
        mvActualToGolden entryInDir
        go entryInDir

mvActualToGolden :: FilePath -> IO ()
mvActualToGolden testPath =
  let actualFilePath = testPath ++ "/actual"
      goldenFilePath = testPath ++ "/golden"
   in do
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

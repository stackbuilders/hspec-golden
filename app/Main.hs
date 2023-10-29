module Main where

import           System.Console.ANSI
import           Control.Monad         (forM_, when)
import           Data.Version          (showVersion)
import           Paths_hspec_golden    (version)
import           Options.Applicative
import           Data.Monoid ((<>))
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

putStrColor :: Color -> String -> IO ()
putStrColor color str = do
  setSGR [SetColor Foreground Dull color]
  putStr str
  setSGR [Reset]

putStrLnColor :: Color -> String -> IO ()
putStrLnColor color str = do
  setSGR [SetColor Foreground Dull color]
  putStrLn str
  setSGR [Reset]

-- Update Files
updateGolden :: FilePath -> IO ()
updateGolden dir = do
  putStrLnColor Yellow "Replacing golden with actual:"
  go dir
  putStrLnColor Green "Finished!"
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
     when actualFileExist (do
       putStr "  Replacing file: "
       putStrColor Yellow goldenFilePath
       putStr " with: "
       putStrColor Green actualFilePath
       putStrLn ""
       renameFile actualFilePath goldenFilePath)


-- Main

main :: IO ()
main = updateGolden =<< execParser opts
  where

  opts = info ((updateDir <$> params) <**> versionOpt <**> helper)
        ( fullDesc <> header "Update your golden files" )

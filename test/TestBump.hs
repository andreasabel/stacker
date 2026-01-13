module TestBump (bumpTestsIO) where

import Test.Tasty ( TestTree )
import Test.Tasty.Golden ( goldenVsFileDiff )
import System.FilePath ((</>))
import System.Directory (copyFile, setCurrentDirectory, getCurrentDirectory)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Control.Monad (forM_)
import Data.Maybe (catMaybes)

-- Import from library
import StackYaml (findStackYamlFiles, getSymlinkMap, applyAction)
import Analysis (analyzeStackYaml)
import CSV (loadSnapshotDB, ensureCSVFiles)

-- | Generate bump tests - all files are bumped in one temp directory
bumpTestsIO :: IO [TestTree]
bumpTestsIO = do
  -- Ensure CSV files are available
  ensureCSVFiles

  -- Create a temp directory for this test run
  sysTempDir <- getCanonicalTemporaryDirectory
  tempDir <- createTempDirectory sysTempDir "stacker-test"

  -- Find all stack*.yaml files using library function
  cwd <- getCurrentDirectory
  setCurrentDirectory "test/tests"
  stackYamlFiles <- findStackYamlFiles
  setCurrentDirectory cwd

  -- Copy all files to temp directory
  forM_ stackYamlFiles $ \file -> do
    copyFile ("test/tests" </> file) (tempDir </> file)

  -- Run bump logic using library functions
  setCurrentDirectory tempDir

  -- Load snapshot database
  db <- loadSnapshotDB

  -- Get symlink map
  symlinkMap <- getSymlinkMap stackYamlFiles

  -- Get and run actions for each file
  actions <- catMaybes <$>  mapM (analyzeStackYaml db symlinkMap) stackYamlFiles
  mapM_ (applyAction False) actions

  setCurrentDirectory cwd

  -- Create a golden test for each file
  return $ map (makeGoldenTest tempDir) stackYamlFiles
  where

    makeGoldenTest tempDir file = goldenVsFileDiff
      ("bump " ++ file)
      (\ref new -> ["diff", "-u", ref, new])
      ("test/golden/tests" </> file)
      (tempDir </> file)
      (return ())  -- Action is empty since bump already ran

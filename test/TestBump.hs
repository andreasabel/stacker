module TestBump (bumpTestsIO) where

import Test.Tasty ( TestTree )
import Test.Tasty.Golden ( goldenVsFileDiff )
import System.FilePath ((</>))
import System.Directory (listDirectory, copyFile, setCurrentDirectory, getCurrentDirectory)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Control.Monad (forM_)
import Data.List (sort)

-- Import from library
import StackYaml (isStackYaml, findStackYamlFiles, getSymlinkMap, applyAction)
import Analysis (analyzeStackYaml)
import CSV (loadSnapshotDB, ensureCSVFiles)
import Data.Map.Strict qualified as Map

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

  -- Analyze and apply actions for each file
  forM_ stackYamlFiles $ \file -> do
    -- Skip if this file is a symlink to another stack*.yaml
    case Map.lookup file symlinkMap of
      Just _ -> return ()  -- Skip symlinks
      Nothing -> do
        maybeAction <- analyzeStackYaml db symlinkMap file
        case maybeAction of
          Just action -> applyAction action
          Nothing -> return ()

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

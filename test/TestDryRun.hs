module TestDryRun (dryRunTests) where

import Test.Tasty
import Test.Tasty.Golden
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.Printf (printf)

-- Import from library
import StackYaml (findStackYamlFiles, getSymlinkMap)
import Analysis (analyzeStackYaml)
import CSV (loadSnapshotDB, ensureCSVFiles)
import Types (Action(..), Snapshot(..))
import Data.Map.Strict qualified as Map

dryRunTests :: [TestTree]
dryRunTests =
  [ goldenVsString
      "dry-run output"
      "test/golden/dry-run.golden"
      runDryRunTest
  ]

runDryRunTest :: IO BSL.ByteString
runDryRunTest = do
  -- Ensure CSV files are available
  ensureCSVFiles

  cwd <- getCurrentDirectory
  setCurrentDirectory "test/tests"

  -- Use library functions to generate dry-run output
  stackYamlFiles <- findStackYamlFiles
  db <- loadSnapshotDB
  symlinkMap <- getSymlinkMap stackYamlFiles

  -- Analyze each file and collect actions
  actions <- mapM (analyzeFile db symlinkMap) stackYamlFiles

  -- Format output like the real dry-run command
  let output = unlines $ map formatAction $ concat actions

  setCurrentDirectory cwd
  -- Properly encode as UTF-8
  return $ BSL.fromStrict $ TE.encodeUtf8 $ T.pack output
  where
    analyzeFile db symlinkMap file = do
      maybeAction <- analyzeStackYaml db symlinkMap file
      return $ maybe [] (:[]) maybeAction

    formatAction action =
      let file = actionFile action
          oldSnap = actionOldSnapshot action
          symlinkMsg = case actionSymlinkTarget action of
            Just target -> printf "%-20s %-25s = symlink to %s" file (T.unpack oldSnap) target
            Nothing -> case actionNewSnapshot action of
              Just newSnap -> printf "%-20s %-25s → bump to %s" file (T.unpack oldSnap) (T.unpack newSnap)
              Nothing -> printf "%-20s %-25s ✓ up to date" file (T.unpack oldSnap)
      in symlinkMsg

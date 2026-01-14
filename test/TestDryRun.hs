module TestDryRun (dryRunTests) where

import Test.Tasty
import Test.Tasty.Golden
import System.Process (readProcess)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.List (sort)

dryRunTests :: [TestTree]
dryRunTests =
  [ goldenVsString
      "dry-run output"
      "test/golden/dry-run.golden"
      runDryRunTest
  , goldenVsString
      "dry-run with specific files"
      "test/golden/dry-run-files.golden"
      runDryRunWithFilesTest
  , goldenVsString
      "dry-run with recursive flag"
      "test/golden/dry-run-recursive.golden"
      runDryRunRecursiveTest
  ]

runDryRunTest :: IO BSL.ByteString
runDryRunTest = runDryRun ["dry-run", "--color=never"]

runDryRunWithFilesTest :: IO BSL.ByteString
runDryRunWithFilesTest = runDryRun ["dry-run", "--color=never", "stack-9.6.yaml", "stack-9.8.yaml"]

runDryRunRecursiveTest :: IO BSL.ByteString
runDryRunRecursiveTest = do
  cwd <- getCurrentDirectory
  setCurrentDirectory "test/recursive"
  output <- readProcess "stacker" ["dry-run", "--recursive", "--color=never"] ""
  setCurrentDirectory cwd
  -- Sort the output lines for consistent comparison
  let sortedLines = T.unlines $ sort $ T.lines $ T.pack output
  return $ BSL.fromStrict $ TE.encodeUtf8 sortedLines

-- | Helper to run dry-run with specified arguments
runDryRun :: [String] -> IO BSL.ByteString
runDryRun args = do
  cwd <- getCurrentDirectory
  setCurrentDirectory "test/tests"
  output <- readProcess "stacker" args ""
  setCurrentDirectory cwd
  -- Properly encode as UTF-8
  return $ BSL.fromStrict $ TE.encodeUtf8 $ T.pack output

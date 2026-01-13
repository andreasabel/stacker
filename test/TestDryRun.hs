module TestDryRun (dryRunTests) where

import Test.Tasty
import Test.Tasty.Golden
import System.Process (readCreateProcess, proc)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import System.Environment (setEnv, unsetEnv)
import Control.Exception (bracket_)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

dryRunTests :: [TestTree]
dryRunTests =
  [ goldenVsString
      "dry-run output"
      "test/golden/dry-run.golden"
      runDryRunTest
  , goldenVsString
      "dry-run with NO_COLOR"
      "test/golden/dry-run.golden"
      runDryRunTestWithNoColor
  ]

runDryRunTest :: IO BSL.ByteString
runDryRunTest = do
  cwd <- getCurrentDirectory
  setCurrentDirectory "test/tests"
  output <- readCreateProcess (proc "stacker" ["dry-run", "--color=never"]) ""
  setCurrentDirectory cwd
  -- Properly encode as UTF-8
  return $ BSL.fromStrict $ TE.encodeUtf8 $ T.pack output

runDryRunTestWithNoColor :: IO BSL.ByteString
runDryRunTestWithNoColor = do
  cwd <- getCurrentDirectory
  setCurrentDirectory "test/tests"
  -- Test that NO_COLOR environment variable works with --color=auto (default)
  -- Use bracket_ to ensure cleanup even if test fails
  output <- bracket_
    (setEnv "NO_COLOR" "1")
    (unsetEnv "NO_COLOR")
    (readCreateProcess (proc "stacker" ["dry-run"]) "")
  setCurrentDirectory cwd
  -- Properly encode as UTF-8
  return $ BSL.fromStrict $ TE.encodeUtf8 $ T.pack output

module TestDryRun (dryRunTests) where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath ((</>))
import System.Process (readProcess)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

dryRunTests :: [TestTree]
dryRunTests =
  [ goldenVsString
      "dry-run output"
      "test/golden/dry-run.txt"
      runDryRunTest
  ]

runDryRunTest :: IO BSL.ByteString
runDryRunTest = do
  cwd <- getCurrentDirectory
  setCurrentDirectory "test/tests"
  output <- readProcess "stack-snapshots" ["dry-run"] ""
  setCurrentDirectory cwd
  -- Properly encode as UTF-8
  return $ BSL.fromStrict $ TE.encodeUtf8 $ T.pack output

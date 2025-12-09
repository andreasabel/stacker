module TestDryRun (dryRunTests) where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath ((</>))

dryRunTests :: [TestTree]
dryRunTests = []
-- Golden tests would be added here in the format:
-- [ goldenVsFile "test name" goldenFile outputFile testAction
-- ]

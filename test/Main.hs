module Main where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import TestAnalysis ( analysisTests )
import TestStackYaml ( stackYamlTests )
import TestDryRun ( dryRunTests )
import TestBump ( bumpTestsIO )

main :: IO ()
main = do
  bumpTests <- bumpTestsIO
  defaultMain $ tests bumpTests

tests :: [TestTree] -> TestTree
tests bumpTests = testGroup "stack-snapshots tests"
  [ testGroup "analysis tests" analysisTests
  , testGroup "stack.yaml parsing tests" stackYamlTests
  , testGroup "dry-run tests" dryRunTests
  , testGroup "bump tests" bumpTests
  ]

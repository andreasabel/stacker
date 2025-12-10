module Main where

import Test.Tasty
import TestDryRun
import TestBump

main :: IO ()
main = do
  bumpTests <- bumpTestsIO
  defaultMain $ tests bumpTests

tests :: [TestTree] -> TestTree
tests bumpTests = testGroup "stack-snapshots tests"
  [ testGroup "dry-run tests" dryRunTests
  , testGroup "bump tests" bumpTests
  ]

module Main where

import Test.Tasty
import TestDryRun
import TestBump

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "stack-snapshots tests"
  [ testGroup "dry-run tests" dryRunTests
  , testGroup "bump tests" bumpTests
  ]

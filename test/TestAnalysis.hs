{-# LANGUAGE OverloadedStrings #-}

module TestAnalysis (analysisTests) where

import Data.Map.Strict qualified as Map
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Analysis (determineNewSnapshot)
import Types

-- | A small, fixed snapshot database, independent of the user's state directory.
testDB :: SnapshotDB
testDB = SnapshotDB
  { dbLTS = Map.fromList
      [ (LTSVersion 24 1, GHCVersion 9 10 2)
      , (LTSVersion 24 5, GHCVersion 9 10 3)
      ]
  , dbNightly = Map.fromList
      [ (NightlyVersion 2025 5 9, GHCVersion 9 10 2)
      , (NightlyVersion 2026 8 27, GHCVersion 9 12 4)
      -- No nightly for 2026-08-28 and 2026-08-29.
      , (NightlyVersion 2026 8 30, GHCVersion 9 12 4)
      , (NightlyVersion 2026 10 12, GHCVersion 9 12 4)
      ]
  , dbGHC = Map.empty
  }

analysisTests :: [TestTree]
analysisTests =
  [ testCase "nightly with LTS for its GHC bumps to LTS" $
      determineNewSnapshot testDB "nightly-2025-05-09" @?= BumpTo "lts-24.5"
  , testCase "known nightly bumps to latest nightly" $
      determineNewSnapshot testDB "nightly-2026-08-27" @?= BumpTo "nightly-2026-10-12"
  , testCase "nightly missing from database bumps to latest nightly" $
      determineNewSnapshot testDB "nightly-2026-08-28" @?= BumpTo "nightly-2026-10-12"
  , testCase "latest nightly is up to date" $
      determineNewSnapshot testDB "nightly-2026-10-12" @?= UpToDate
  , testCase "nightly newer than database is unknown" $
      determineNewSnapshot testDB "nightly-2026-10-20" @?= Unknown
  , testCase "LTS bumps to latest minor" $
      determineNewSnapshot testDB "lts-24.1" @?= BumpTo "lts-24.5"
  , testCase "latest LTS is up to date" $
      determineNewSnapshot testDB "lts-24.5" @?= UpToDate
  , testCase "LTS newer than database is unknown" $
      determineNewSnapshot testDB "lts-24.9" @?= Unknown
  , testCase "LTS with unknown major is unknown" $
      determineNewSnapshot testDB "lts-99.1" @?= Unknown
  , testCase "non-Stackage resolver is unknown" $
      determineNewSnapshot testDB "ghc-9.12.4" @?= Unknown
  ]

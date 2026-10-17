{-# LANGUAGE OverloadedStrings #-}

module TestStackYaml (stackYamlTests) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import StackYaml (findSnapshot, fromStackageURL, toStackageURL)

stackYamlTests :: [TestTree]
stackYamlTests =
  [ testCase "snapshot field" $
      findSnapshot "snapshot: lts-24.5\n" @?= Just ("lts-24.5", False, (10, 18))
  , testCase "resolver field" $
      findSnapshot "resolver: lts-24.5\n" @?= Just ("lts-24.5", True, (10, 18))
  , testCase "snapshot takes precedence over resolver" $
      findSnapshot "resolver: lts-23.0\nsnapshot: lts-24.5\n"
        @?= Just ("lts-24.5", False, (29, 37))
  , testCase "commented-out snapshot is ignored" $
      findSnapshot "# snapshot: lts-22.0\nresolver: lts-24.5\n"
        @?= Just ("lts-24.5", True, (31, 39))
  , testCase "commented-out resolver is ignored" $
      findSnapshot "snapshot: lts-24.5\n#resolver: lts-22.0\n"
        @?= Just ("lts-24.5", False, (10, 18))
  , testCase "only commented-out fields" $
      findSnapshot "# snapshot: lts-22.0\n# resolver: lts-22.0\n" @?= Nothing
  , testCase "commented-out field before the real one" $
      findSnapshot "#snapshot: lts-22.0\nsnapshot: lts-24.5\n"
        @?= Just ("lts-24.5", False, (30, 38))
  , testCase "nested key is ignored" $
      findSnapshot "foo:\n  snapshot: lts-22.0\nsnapshot: lts-24.5\n"
        @?= Just ("lts-24.5", False, (36, 44))
  , testCase "key with prefix is ignored" $
      findSnapshot "mysnapshot: lts-22.0\nsnapshot: lts-24.5\n"
        @?= Just ("lts-24.5", False, (31, 39))
  , testCase "trailing comment is not part of the value" $
      findSnapshot "snapshot: lts-24.5  # pinned\n" @?= Just ("lts-24.5", False, (10, 18))
  , testCase "# without preceding space is part of the value" $
      findSnapshot "snapshot: lts-24.5#x\n" @?= Just ("lts-24.5#x", False, (10, 20))
  , testCase "value consisting only of a comment" $
      findSnapshot "snapshot: # none\nresolver: lts-24.5\n"
        @?= Just ("lts-24.5", True, (27, 35))
  , testCase "CRLF line endings" $
      findSnapshot "# snapshot: x\r\nsnapshot: lts-24.5\r\n"
        @?= Just ("lts-24.5", False, (25, 33))
  , testCase "nested url" $
      findSnapshot "snapshot:\n  url: https://x/lts/24/63.yaml\n"
        @?= Just ("https://x/lts/24/63.yaml", False, (17, 41))
  , testCase "nested url after comments and blank lines" $
      findSnapshot "snapshot:\n  # comment\n\n  url: u # c\npackages:\n"
        @?= Just ("u", False, (30, 31))
  , testCase "nested url with CRLF line endings" $
      findSnapshot "snapshot:\r\n  url: u\r\n" @?= Just ("u", False, (18, 19))
  , testCase "url outside of the snapshot block is ignored" $
      findSnapshot "snapshot:\npackages:\n  url: u\n" @?= Nothing
  , testCase "nested url under resolver" $
      findSnapshot "resolver:\n  url: u\n" @?= Just ("u", True, (17, 18))
  , testCase "LTS URL to name" $
      fromStackageURL (prefix <> "lts/24/63.yaml") @?= Just "lts-24.63"
  , testCase "nightly URL to name" $
      fromStackageURL (prefix <> "nightly/2026/1/5.yaml") @?= Just "nightly-2026-01-05"
  , testCase "URL from other repository is not recognized" $
      fromStackageURL "https://example.com/lts/24/63.yaml" @?= Nothing
  , testCase "URL from other branch is not recognized" $
      fromStackageURL "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/abc123/lts/24/63.yaml"
        @?= Nothing
  , testCase "malformed LTS URL is not recognized" $
      fromStackageURL (prefix <> "lts/24/x.yaml") @?= Nothing
  , testCase "LTS name to URL" $
      toStackageURL "lts-24.63" @?= Just (prefix <> "lts/24/63.yaml")
  , testCase "nightly name to URL" $
      toStackageURL "nightly-2026-01-05" @?= Just (prefix <> "nightly/2026/1/5.yaml")
  , testCase "other name has no URL" $
      toStackageURL "ghc-9.10.3" @?= Nothing
  ]
  where
    prefix = "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/"

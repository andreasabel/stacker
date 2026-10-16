{-# LANGUAGE OverloadedStrings #-}

module TestStackYaml (stackYamlTests) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import StackYaml (findSnapshot)

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
  ]

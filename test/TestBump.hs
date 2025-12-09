module TestBump (bumpTests) where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath ((</>))

bumpTests :: [TestTree]
bumpTests = []
-- Golden tests would be added here in the format:
-- [ goldenVsFile "test name" goldenFile outputFile testAction
-- ]

module TestBump (bumpTestsIO) where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath ((</>), takeFileName)
import System.Process (callProcess)
import System.Directory (setCurrentDirectory, getCurrentDirectory, copyFile, removeFile, listDirectory)
import Control.Exception (bracket_)
import Data.List (isSuffixOf, sort)

-- | Generate bump tests by finding all stack*.yaml files in test/tests
bumpTestsIO :: IO [TestTree]
bumpTestsIO = do
  -- Find all .yaml files in test/tests that start with "stack"
  allFiles <- listDirectory "test/tests"
  let stackYamlFiles = sort $ filter (\f -> "stack" `isPrefixOf` f && ".yaml" `isSuffixOf` f) allFiles
  return 
    [ goldenVsFileDiff
        ("bump " ++ file)
        (\ref new -> ["diff", "-u", ref, new])
        ("test/golden/tests" </> file)
        ("test/tests" </> file)
        (runBumpTest file)
    | file <- stackYamlFiles
    ]
  where
    isPrefixOf = isPrefixOf'
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

runBumpTest :: FilePath -> IO ()
runBumpTest file = do
  cwd <- getCurrentDirectory
  let testFile = "test/tests" </> file
  let backupFile = testFile ++ ".backup"
  
  -- Backup the file first
  copyFile testFile backupFile
  
  -- Run bump in test directory
  bracket_
    (setCurrentDirectory "test/tests")
    (setCurrentDirectory cwd >> copyFile backupFile testFile >> removeFile backupFile)
    (callProcess "stack-snapshots" ["bump"])

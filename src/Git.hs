{-# LANGUAGE ScopedTypeVariables #-}

module Git
  ( ensureRepo
  , updateRepo
  , getCurrentBranch
  ) where

import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Process (readProcess, callProcess)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)

stackageSnapshotsUrl :: String
stackageSnapshotsUrl = "https://github.com/commercialhaskell/stackage-snapshots"

-- | Ensure the repository exists, cloning it if necessary
ensureRepo :: FilePath -> IO ()
ensureRepo repoPath = do
  exists <- doesDirectoryExist repoPath
  if exists
    then return ()
    else cloneRepo repoPath

-- | Clone the stackage-snapshots repository
cloneRepo :: FilePath -> IO ()
cloneRepo repoPath = do
  createDirectoryIfMissing True (takeDirectory repoPath)
  putStrLn $ "Cloning " ++ stackageSnapshotsUrl ++ " to " ++ repoPath ++ "..."
  callProcess "git" ["clone", "--depth", "1", stackageSnapshotsUrl, repoPath]
  putStrLn "Clone complete."

-- | Get the current branch name
getCurrentBranch :: FilePath -> IO String
getCurrentBranch repoPath = do
  result <- readProcess "git" ["-C", repoPath, "rev-parse", "--abbrev-ref", "HEAD"] ""
  return $ trim result

-- | Update the repository by pulling
updateRepo :: FilePath -> IO ()
updateRepo repoPath = do
  branch <- getCurrentBranch repoPath
  if branch /= "master"
    then do
      putStrLn $ "Error: Repository is on branch '" ++ branch ++ "', not 'master'."
      putStrLn "Cannot update repository when not on master branch."
      exitFailure
    else do
      putStrLn $ "Updating repository at " ++ repoPath ++ "..."
      catch
        (callProcess "git" ["-C", repoPath, "pull"])
        (\(e :: SomeException) -> do
          putStrLn $ "Warning: Failed to update repository: " ++ show e
          return ())
      putStrLn "Update complete."

-- | Trim whitespace from a string
trim :: String -> String
trim = reverse . dropWhile (`elem` " \t\n\r") . reverse . dropWhile (`elem` " \t\n\r")

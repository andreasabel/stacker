module XDG
  ( getStateDir
  , getConfigDir
  , getConfigFile
  , getRepoDir
  ) where

import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))
import Types (appName)

-- | Get the XDG state directory for the application
getStateDir :: IO FilePath
getStateDir = getXdgDirectory XdgState appName

-- | Get the XDG config directory for the application
getConfigDir :: IO FilePath
getConfigDir = getXdgDirectory XdgConfig appName

-- | Get the path to the config file
getConfigFile :: IO FilePath
getConfigFile = do
  dir <- getConfigDir
  return $ dir </> "config.yaml"

-- | Get the default repository directory
getRepoDir :: IO FilePath
getRepoDir = do
  stateDir <- getStateDir
  return $ stateDir </> "stackage-snapshots"

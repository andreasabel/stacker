{-# LANGUAGE OverloadedStrings #-}

module Config
  ( loadConfig
  , saveConfig
  , getRepoPath
  ) where

import Data.Yaml (decodeFileEither, encodeFile, FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject)
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import System.FilePath (takeDirectory)
import Types (AppConfig(..))
import XDG (getConfigFile, getRepoDir)

instance FromJSON AppConfig where
  parseJSON = withObject "AppConfig" $ \v ->
    AppConfig <$> v .:? "repo"

instance ToJSON AppConfig where
  toJSON (AppConfig repo) = object ["repo" .= repo]

-- | Load configuration from the config file
loadConfig :: IO AppConfig
loadConfig = do
  configFile <- getConfigFile
  exists <- doesFileExist configFile
  if exists
    then do
      result <- decodeFileEither configFile
      case result of
        Left _ -> return $ AppConfig Nothing
        Right config -> return config
    else return $ AppConfig Nothing

-- | Save configuration to the config file
saveConfig :: AppConfig -> IO ()
saveConfig config = do
  configFile <- getConfigFile
  createDirectoryIfMissing True (takeDirectory configFile)
  encodeFile configFile config

-- | Get the repository path, either from config or default
getRepoPath :: IO FilePath
getRepoPath = do
  config <- loadConfig
  case configRepo config of
    Just repo -> return repo
    Nothing -> getRepoDir

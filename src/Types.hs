{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | LTS version (e.g., "24.23" for lts-24.23)
data LTSVersion = LTSVersion
  { ltsMajor :: Int
  , ltsMinor :: Int
  } deriving (Eq, Ord, Show, Generic)

-- | Nightly version (e.g., "2025-12-09" for nightly-2025-12-09)
newtype NightlyVersion = NightlyVersion Text
  deriving (Eq, Ord, Show, Generic)

-- | GHC version (e.g., "9.10.1")
newtype GHCVersion = GHCVersion Text
  deriving (Eq, Ord, Show, Generic)

-- | Snapshot reference
data Snapshot
  = LTS LTSVersion
  | Nightly NightlyVersion
  deriving (Eq, Ord, Show, Generic)

-- | Action to take for a stack*.yaml file
data Action = Action
  { actionFile :: FilePath
  , actionOldSnapshot :: Text
  , actionNewSnapshot :: Maybe Text
  , actionIsResolver :: Bool  -- True if 'resolver' field, False if 'snapshot'
  , actionSpan :: (Int, Int)  -- Character span to replace
  } deriving (Eq, Show, Generic)

-- | Database of snapshots
data SnapshotDB = SnapshotDB
  { dbLTS :: Map LTSVersion GHCVersion
  , dbNightly :: Map NightlyVersion GHCVersion
  , dbGHC :: Map GHCVersion Snapshot
  } deriving (Eq, Show, Generic)

-- | Configuration
data AppConfig = AppConfig
  { configRepo :: Maybe FilePath
  } deriving (Eq, Show, Generic)

-- | Command-line command
data Command
  = Bump
  | DryRun
  | Config ConfigCmd
  | Update
  | Info
  | Version
  | NumericVersion
  | PrintLicense
  | Help
  deriving (Eq, Show)

-- | Config command
data ConfigCmd
  = SetRepo FilePath
  deriving (Eq, Show)

-- | Color option
data ColorWhen
  = Always
  | Never
  | Auto
  deriving (Eq, Show, Read)

-- | Options
data Options = Options
  { optCommand :: Command
  , optColor :: ColorWhen
  } deriving (Eq, Show)

-- | Application version
appVersion :: String
appVersion = "0.1.0.0"

-- | Application name
appName :: String
appName = "stack-snapshots"

-- | Copyright information
copyright :: String
copyright = "Copyright (c) 2024 Andreas Abel"

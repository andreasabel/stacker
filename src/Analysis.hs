{-# LANGUAGE OverloadedStrings #-}

module Analysis
  ( analyzeStackYaml
  , analyzeAllStackYamls
  , analyzeStackYamls
  ) where

import Prelude hiding (min, span)

import Control.Monad (filterM)
import Data.Functor ((<&>))
import Data.List (maximumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesFileExist)
import Text.Printf (printf)

import Types (Action(..), SnapshotDB(..), LTSVersion(..), NightlyVersion(..), GHCVersion(..), Snapshot(..))
import StackYaml (parseStackYaml, findStackYamlFiles, getSymlinkMap)

-- | Analyze a single stack.yaml file
analyzeStackYaml :: SnapshotDB -> Map.Map FilePath FilePath -> FilePath -> IO (Maybe Action)
analyzeStackYaml db symlinkMap file = do
  -- Check if this file is a symlink to another stack*.yaml file in our list
  let symlinkTarget = Map.lookup file symlinkMap

  parseStackYaml file <&> \case
    Nothing -> Nothing
    Just (oldSnap, isResolver, span) -> do
      let newSnap = determineNewSnapshot db oldSnap
      Just $ Action file oldSnap newSnap isResolver span symlinkTarget

-- | Analyze specific stack*.yaml files, or all if empty list
analyzeStackYamls :: SnapshotDB -> [FilePath] -> IO [Action]
analyzeStackYamls db files = do
  filesToAnalyze <- if null files 
                    then findStackYamlFiles 
                    else filterM doesFileExist files  -- Validate user-provided files exist
  symlinkMap <- getSymlinkMap filesToAnalyze
  results <- mapM (analyzeStackYaml db symlinkMap) filesToAnalyze
  return $ catMaybes results

-- | Analyze all stack*.yaml files in the current directory
analyzeAllStackYamls :: SnapshotDB -> IO [Action]
analyzeAllStackYamls db = analyzeStackYamls db []

-- | Determine the new snapshot for a given old snapshot
determineNewSnapshot :: SnapshotDB -> Text -> Maybe Text
determineNewSnapshot db oldSnap
  | T.isPrefixOf "lts-" oldSnap = determineLTSBump db oldSnap
  | T.isPrefixOf "nightly-" oldSnap = determineNightlyBump db oldSnap
  | otherwise = Nothing

-- | Determine bump for LTS snapshot
determineLTSBump :: SnapshotDB -> Text -> Maybe Text
determineLTSBump db oldSnap = do
  oldVersion <- parseLTSSnapshot oldSnap
  -- Find the latest LTS with the same major version
  let sameMajor = filter (\(LTSVersion maj _, _) -> maj == ltsMajor oldVersion) $ Map.toList (dbLTS db)
  if null sameMajor
    then Nothing
    else do
      let (latestVersion, _) = maximumBy (comparing fst) sameMajor
      let newSnap = formatSnapshot (LTS latestVersion)
      if newSnap == oldSnap
        then Nothing  -- Already up to date
        else Just newSnap

-- | Determine bump for nightly snapshot
determineNightlyBump :: SnapshotDB -> Text -> Maybe Text
determineNightlyBump db oldSnap = do
  oldVersion <- parseNightlySnapshot oldSnap
  oldGHC <- Map.lookup oldVersion (dbNightly db)
  -- Get GHC major version (e.g., (9,6) from GHCVersion 9 6 1)
  let ghcMajor = getGHCMajor oldGHC
  -- Find if there's an LTS for this GHC major version
  let ltsForGHC = filter (\(_, ghc) -> getGHCMajor ghc == ghcMajor) $ Map.toList (dbLTS db)
  if not (null ltsForGHC)
    then do
      -- Bump to latest LTS for this GHC major
      let (latestLTS, _) = maximumBy (comparing fst) ltsForGHC
      return $ formatSnapshot (LTS latestLTS)
    else do
      -- Bump to latest nightly for this GHC major
      let nightliesForGHC = filter (\(_, ghc) -> getGHCMajor ghc == ghcMajor) $ Map.toList (dbNightly db)
      if null nightliesForGHC
        then Nothing
        else do
          let (latestNightly, _) = maximumBy (comparing fst) nightliesForGHC
          let newSnap = formatSnapshot (Nightly latestNightly)
          if newSnap == oldSnap
            then Nothing
            else Just newSnap

-- | Get GHC major version from full version (e.g., (9,6) from GHCVersion 9 6 1)
getGHCMajor :: GHCVersion -> (Int, Int)
getGHCMajor (GHCVersion maj1 maj2 _) = (maj1, maj2)

-- | Parse LTS snapshot string
parseLTSSnapshot :: Text -> Maybe LTSVersion
parseLTSSnapshot snap =
  case T.splitOn "." (T.drop 4 snap) of  -- Remove "lts-"
    [majStr, minStr] ->
      case (reads $ T.unpack majStr, reads $ T.unpack minStr) of
        ([(maj, "")], [(min, "")]) -> Just $ LTSVersion maj min
        _ -> Nothing
    _ -> Nothing

-- | Parse nightly snapshot string
parseNightlySnapshot :: Text -> Maybe NightlyVersion
parseNightlySnapshot snap =
  case T.splitOn "-" (T.drop 8 snap) of  -- Remove "nightly-"
    [yearStr, monthStr, dayStr] ->
      case (reads $ T.unpack yearStr, reads $ T.unpack monthStr, reads $ T.unpack dayStr) of
        ([(year, "")], [(month, "")], [(day, "")]) -> Just $ NightlyVersion year month day
        _ -> Nothing
    _ -> Nothing

-- | Format a snapshot as a string
formatSnapshot :: Snapshot -> Text
formatSnapshot (LTS (LTSVersion maj min)) =
  "lts-" <> T.pack (show maj) <> "." <> T.pack (show min)
formatSnapshot (Nightly (NightlyVersion year month day)) =
  T.pack $ "nightly-" ++ printf "%d-%02d-%02d" year month day

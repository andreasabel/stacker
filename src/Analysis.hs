{-# LANGUAGE OverloadedStrings #-}

module Analysis
  ( analyzeStackYaml
  , analyzeAllStackYamls
  ) where

import Prelude hiding (min, span)

import Data.List (maximumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Types (Action(..), SnapshotDB(..), LTSVersion(..), NightlyVersion(..), GHCVersion(..), Snapshot(..))
import StackYaml (parseStackYaml, findStackYamlFiles)

-- | Analyze a single stack.yaml file
analyzeStackYaml :: SnapshotDB -> FilePath -> IO (Maybe Action)
analyzeStackYaml db file = do
  result <- parseStackYaml file
  case result of
    Nothing -> return Nothing
    Just (oldSnap, isResolver, span) -> do
      let newSnap = determineNewSnapshot db oldSnap
      return $ Just $ Action file oldSnap newSnap isResolver span

-- | Analyze all stack*.yaml files in the current directory
analyzeAllStackYamls :: SnapshotDB -> IO [Action]
analyzeAllStackYamls db = do
  files <- findStackYamlFiles
  results <- mapM (analyzeStackYaml db) files
  return $ concat $ map maybeToList results
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

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

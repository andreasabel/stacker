{-# LANGUAGE OverloadedStrings #-}

module Analysis
  ( analyzeStackYaml
  , analyzeAllStackYamls
  ) where

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
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
  oldGHC <- Map.lookup oldVersion (dbLTS db)
  newSnapshot <- Map.lookup oldGHC (dbGHC db)
  let newSnap = formatSnapshot newSnapshot
  if newSnap == oldSnap
    then Nothing  -- Already up to date
    else Just newSnap

-- | Determine bump for nightly snapshot
determineNightlyBump :: SnapshotDB -> Text -> Maybe Text
determineNightlyBump db oldSnap = do
  oldVersion <- parseNightlySnapshot oldSnap
  oldGHC <- Map.lookup oldVersion (dbNightly db)
  newSnapshot <- Map.lookup oldGHC (dbGHC db)
  let newSnap = formatSnapshot newSnapshot
  if newSnap == oldSnap
    then Nothing  -- Already up to date
    else Just newSnap

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
  Just $ NightlyVersion $ T.drop 8 snap  -- Remove "nightly-"

-- | Format a snapshot as a string
formatSnapshot :: Snapshot -> Text
formatSnapshot (LTS (LTSVersion maj min)) =
  "lts-" <> T.pack (show maj) <> "." <> T.pack (show min)
formatSnapshot (Nightly (NightlyVersion date)) =
  "nightly-" <> date

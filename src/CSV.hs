{-# LANGUAGE OverloadedStrings #-}

module CSV
  ( generateCSVs
  , loadSnapshotDB
  ) where

import Control.Monad (forM_)
import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing, Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension)
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:))
import qualified Data.Aeson as Aeson
import Types (LTSVersion(..), NightlyVersion(..), GHCVersion(..), Snapshot(..), SnapshotDB(..))
import XDG (getStateDir)

-- | Generate CSV files from the stackage-snapshots repository
generateCSVs :: FilePath -> IO ()
generateCSVs repoPath = do
  stateDir <- getStateDir
  createDirectoryIfMissing True stateDir
  
  -- Process LTS snapshots
  ltsMap <- processLTSSnapshots (repoPath </> "lts")
  writeLTSCSV (stateDir </> "lts.csv") ltsMap
  
  -- Process nightly snapshots
  nightlyMap <- processNightlySnapshots (repoPath </> "nightly")
  writeNightlyCSV (stateDir </> "nightly.csv") nightlyMap
  
  -- Generate GHC version map
  let ghcMap = generateGHCMap ltsMap nightlyMap
  writeGHCCSV (stateDir </> "ghc.csv") ghcMap

-- | Process LTS snapshots (structure: lts/major/minor.yaml)
processLTSSnapshots :: FilePath -> IO [(LTSVersion, GHCVersion)]
processLTSSnapshots ltsDir = do
  majorDirs <- listDirectory ltsDir
  results <- mapM (processMajor ltsDir) majorDirs
  return $ concat results
  where
    processMajor dir majorStr = do
      case reads majorStr of
        [(major, "")] -> do
          let majorPath = dir </> majorStr
          minorFiles <- listDirectory majorPath
          mapM (processMinor majorPath major) minorFiles
        _ -> return []
    processMinor dir major fname
      | takeExtension fname == ".yaml" = do
          let minorStr = take (length fname - 5) fname
          case reads minorStr of
            [(minor, "")] -> do
              ghc <- extractGHCVersion (dir </> fname)
              return (LTSVersion major minor, ghc)
            _ -> error $ "Invalid minor version in " ++ fname
      | otherwise = error $ "Unexpected file " ++ fname

-- | Process nightly snapshots (structure: nightly/year/month/day.yaml)
processNightlySnapshots :: FilePath -> IO [(NightlyVersion, GHCVersion)]
processNightlySnapshots nightlyDir = do
  yearDirs <- listDirectory nightlyDir
  results <- mapM (processYear nightlyDir) yearDirs
  return $ concat results
  where
    processYear dir yearStr = do
      let yearPath = dir </> yearStr
      monthDirs <- listDirectory yearPath
      months <- mapM (processMonth yearPath yearStr) monthDirs
      return $ concat months
    processMonth dir year monthStr = do
      let monthPath = dir </> monthStr
      dayFiles <- listDirectory monthPath
      mapM (processDay monthPath year monthStr) dayFiles
    processDay dir year month fname
      | takeExtension fname == ".yaml" = do
          let day = take (length fname - 5) fname
          let dateStr = year ++ "-" ++ month ++ "-" ++ day
          ghc <- extractGHCVersion (dir </> fname)
          return (NightlyVersion $ T.pack dateStr, ghc)
      | otherwise = error $ "Unexpected file " ++ fname

-- | Extract GHC version from a snapshot YAML file
extractGHCVersion :: FilePath -> IO GHCVersion
extractGHCVersion file = do
  result <- Yaml.decodeFileEither file
  case result of
    Left err -> error $ "Failed to parse " ++ file ++ ": " ++ show err
    Right (Aeson.Object obj) ->
      -- Try new format first (nested under resolver)
      case Yaml.parseMaybe (.: "resolver") obj of
        Just (Aeson.Object resolverObj) ->
          case Yaml.parseMaybe (.: "compiler") resolverObj of
            Just (Aeson.String compiler) -> 
              return $ GHCVersion $ T.drop 4 compiler  -- Remove "ghc-" prefix
            _ -> error $ "No compiler field in resolver in " ++ file
        _ ->
          -- Try old format (compiler at top level)
          case Yaml.parseMaybe (.: "compiler") obj of
            Just (Aeson.String compiler) -> 
              return $ GHCVersion $ T.drop 4 compiler  -- Remove "ghc-" prefix
            _ -> error $ "No compiler field in " ++ file
    _ -> error $ "Invalid YAML in " ++ file

-- | Write LTS CSV file
writeLTSCSV :: FilePath -> [(LTSVersion, GHCVersion)] -> IO ()
writeLTSCSV path entries = do
  let sorted = sortBy (comparing fst) entries
  let lines = map formatLTSEntry sorted
  TIO.writeFile path $ T.unlines lines
  where
    formatLTSEntry (LTSVersion maj min, GHCVersion ghc) =
      T.pack (show maj) <> "." <> T.pack (show min) <> "," <> ghc

-- | Write nightly CSV file
writeNightlyCSV :: FilePath -> [(NightlyVersion, GHCVersion)] -> IO ()
writeNightlyCSV path entries = do
  let sorted = sortBy (comparing fst) entries
  let lines = map formatNightlyEntry sorted
  TIO.writeFile path $ T.unlines lines
  where
    formatNightlyEntry (NightlyVersion date, GHCVersion ghc) =
      date <> "," <> ghc

-- | Generate GHC version map from LTS and nightly maps
generateGHCMap :: [(LTSVersion, GHCVersion)] -> [(NightlyVersion, GHCVersion)] -> Map GHCVersion Snapshot
generateGHCMap ltsEntries nightlyEntries =
  let ltsMap = foldr insertLTS Map.empty ltsEntries
      nightlyMap = foldr insertNightly Map.empty nightlyEntries
  in Map.union ltsMap nightlyMap
  where
    insertLTS (lts, ghc) m =
      Map.insertWith (\_ old -> old) ghc (LTS lts) m
    insertNightly (nightly, ghc) m =
      Map.insertWith (\_ old -> old) ghc (Nightly nightly) m

-- | Write GHC CSV file
writeGHCCSV :: FilePath -> Map GHCVersion Snapshot -> IO ()
writeGHCCSV path ghcMap = do
  let sorted = sortBy (comparing fst) $ Map.toList ghcMap
  let lines = map formatGHCEntry sorted
  TIO.writeFile path $ T.unlines lines
  where
    formatGHCEntry (GHCVersion ghc, LTS (LTSVersion maj min)) =
      ghc <> ",lts-" <> T.pack (show maj) <> "." <> T.pack (show min)
    formatGHCEntry (GHCVersion ghc, Nightly (NightlyVersion date)) =
      ghc <> ",nightly-" <> date

-- | Load snapshot database from CSV files
loadSnapshotDB :: IO SnapshotDB
loadSnapshotDB = do
  stateDir <- getStateDir
  
  ltsMap <- readLTSCSV (stateDir </> "lts.csv")
  nightlyMap <- readNightlyCSV (stateDir </> "nightly.csv")
  ghcMap <- readGHCCSV (stateDir </> "ghc.csv")
  
  return $ SnapshotDB ltsMap nightlyMap ghcMap

-- | Read LTS CSV file
readLTSCSV :: FilePath -> IO (Map LTSVersion GHCVersion)
readLTSCSV path = do
  exists <- doesFileExist path
  if not exists
    then return Map.empty
    else do
      content <- TIO.readFile path
      let entries = map parseLTSLine $ T.lines content
      return $ Map.fromList $ concat entries
  where
    parseLTSLine line =
      case T.splitOn "," line of
        [ver, ghc] ->
          case T.splitOn "." ver of
            [majStr, minStr] ->
              case (reads $ T.unpack majStr, reads $ T.unpack minStr) of
                ([(maj, "")], [(min, "")]) ->
                  [(LTSVersion maj min, GHCVersion ghc)]
                _ -> []
            _ -> []
        _ -> []

-- | Read nightly CSV file
readNightlyCSV :: FilePath -> IO (Map NightlyVersion GHCVersion)
readNightlyCSV path = do
  exists <- doesFileExist path
  if not exists
    then return Map.empty
    else do
      content <- TIO.readFile path
      let entries = map parseNightlyLine $ T.lines content
      return $ Map.fromList $ concat entries
  where
    parseNightlyLine line =
      case T.splitOn "," line of
        [date, ghc] -> [(NightlyVersion date, GHCVersion ghc)]
        _ -> []

-- | Read GHC CSV file
readGHCCSV :: FilePath -> IO (Map GHCVersion Snapshot)
readGHCCSV path = do
  exists <- doesFileExist path
  if not exists
    then return Map.empty
    else do
      content <- TIO.readFile path
      let entries = map parseGHCLine $ T.lines content
      return $ Map.fromList $ concat entries
  where
    parseGHCLine line =
      case T.splitOn "," line of
        [ghc, snap] ->
          case parseSnapshot snap of
            Just s -> [(GHCVersion ghc, s)]
            Nothing -> []
        _ -> []
    parseSnapshot snap
      | T.isPrefixOf "lts-" snap =
          case T.splitOn "." (T.drop 4 snap) of
            [majStr, minStr] ->
              case (reads $ T.unpack majStr, reads $ T.unpack minStr) of
                ([(maj, "")], [(min, "")]) -> Just $ LTS $ LTSVersion maj min
                _ -> Nothing
            _ -> Nothing
      | T.isPrefixOf "nightly-" snap =
          Just $ Nightly $ NightlyVersion $ T.drop 8 snap
      | otherwise = Nothing

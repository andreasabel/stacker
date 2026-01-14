{-# LANGUAGE OverloadedStrings #-}

module CSV
  ( generateCSVs
  , loadSnapshotDB
  , ensureCSVFiles
  ) where

import Prelude hiding (lines, min)

import Control.Monad (unless)
import Data.List (sortBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeExtension)
import PathUtil ((</>))
import Data.Yaml qualified as Yaml
import Data.Yaml ((.:))
import Data.Aeson qualified as Aeson
import Text.Printf (printf)
import Types (LTSVersion(..), NightlyVersion(..), GHCVersion(..), Snapshot(..), SnapshotDB(..))
import XDG (getStateDir)
import System.IO (stderr)
import Paths_stacker (getDataDir)
import System.Directory qualified as Dir

-- Helper functions for parsing
parseGHCVersionText :: Text -> Maybe GHCVersion
parseGHCVersionText txt =
  case T.splitOn "." txt of
    [maj1Str, maj2Str, minStr] ->
      case (reads $ T.unpack maj1Str, reads $ T.unpack maj2Str, reads $ T.unpack minStr) of
        ([(maj1, "")], [(maj2, "")], [(minV, "")]) -> Just $ GHCVersion maj1 maj2 minV
        _ -> Nothing
    _ -> Nothing

parseNightlyVersionText :: Text -> Maybe NightlyVersion
parseNightlyVersionText txt =
  case T.splitOn "-" txt of
    [yearStr, monthStr, dayStr] ->
      case (reads $ T.unpack yearStr, reads $ T.unpack monthStr, reads $ T.unpack dayStr) of
        ([(year, "")], [(month, "")], [(day, "")]) -> Just $ NightlyVersion year month day
        _ -> Nothing
    _ -> Nothing

-- Helper functions for formatting
formatGHCVersion :: GHCVersion -> Text
formatGHCVersion (GHCVersion maj1 maj2 minV) =
  T.pack $ show maj1 ++ "." ++ show maj2 ++ "." ++ show minV

formatNightlyVersion :: NightlyVersion -> Text
formatNightlyVersion (NightlyVersion year month day) =
  T.pack $ printf "%d-%02d-%02d" year month day

-- | Ensure CSV files exist in state directory, copying from data directory if needed
ensureCSVFiles :: IO ()
ensureCSVFiles = do
  stateDir <- getStateDir
  createDirectoryIfMissing True stateDir

  dataDir <- getDataDir

  -- Check if any CSV file is missing
  ghcExists <- doesFileExist (stateDir </> "ghc.csv")
  ltsExists <- doesFileExist (stateDir </> "lts.csv")
  nightlyExists <- doesFileExist (stateDir </> "nightly.csv")

  unless (ghcExists && ltsExists && nightlyExists) $ do
    TIO.hPutStrLn stderr "Copying CSV files from data directory..."
    -- Copy all three CSV files from data directory
    Dir.copyFile (dataDir </> "ghc.csv") (stateDir </> "ghc.csv")
    Dir.copyFile (dataDir </> "lts.csv") (stateDir </> "lts.csv")
    Dir.copyFile (dataDir </> "nightly.csv") (stateDir </> "nightly.csv")

-- | Generate CSV files from the stackage-snapshots repository
generateCSVs :: FilePath -> IO ()
generateCSVs repoPath = do
  stateDir <- getStateDir
  createDirectoryIfMissing True stateDir

  TIO.hPutStrLn stderr $ "Processing snapshot database.  This might take a couple of minutes..."

  -- Process LTS snapshots
  TIO.hPutStrLn stderr $ "Processing LTSs..."
  ltsMap <- processLTSSnapshots (repoPath </> "lts")
  writeLTSCSV (stateDir </> "lts.csv") ltsMap

  -- Process nightly snapshots
  TIO.hPutStrLn stderr $ "Processing nightlys..."
  nightlyMap <- processNightlySnapshots (repoPath </> "nightly")
  writeNightlyCSV (stateDir </> "nightly.csv") nightlyMap

  -- Generate GHC version map
  TIO.hPutStrLn stderr $ "Computing latest snapshot per GHC version..."
  let ghcMap = generateGHCMap ltsMap nightlyMap
  writeGHCCSV (stateDir </> "ghc.csv") ghcMap

  TIO.hPutStrLn stderr $ "Done processing snapshot database."

-- | Process LTS snapshots (structure: lts/major/minor.yaml)
processLTSSnapshots :: FilePath -> IO [(LTSVersion, GHCVersion)]
processLTSSnapshots ltsDir = do
  majorDirs <- listDirectory ltsDir
  let sortedMajors = sortOn (\s -> read s :: Int) $ filter (all (`elem` ("0123456789" :: String))) majorDirs
  concat <$> mapM (processMajor ltsDir) sortedMajors
  where
    processMajor dir majorStr = do
      case reads majorStr of
        [(major, "")] -> do
          let majorPath = dir </> majorStr
          minorFiles <- listDirectory majorPath
          let sortedMinors = sortOn (\s -> read (take (length s - 5) s) :: Int) $
                             filter (\s -> takeExtension s == ".yaml") minorFiles
          mapM (processMinor majorPath major) sortedMinors
        _ -> return []
    processMinor dir major fname
      | takeExtension fname == ".yaml" = do
          let minorStr = take (length fname - 5) fname
          case reads minorStr of
            [(minor, "")] -> do
              ghc <- extractGHCVersion (dir </> fname)
              return (LTSVersion major minor, ghc)
            _ -> error $ "Invalid minor version format in " ++ fname ++
                        ": expected integer, got '" ++ minorStr ++ "'"
      | otherwise = error $ "Unexpected file in LTS directory: " ++ fname ++
                           " (expected .yaml extension)"

-- | Process nightly snapshots (structure: nightly/year/month/day.yaml)
processNightlySnapshots :: FilePath -> IO [(NightlyVersion, GHCVersion)]
processNightlySnapshots nightlyDir = do
  yearDirs <- listDirectory nightlyDir
  let sortedYears = sortOn (\s -> read s :: Int) $ filter (all (`elem` ("0123456789" :: String))) yearDirs
  concat <$> mapM (processYear nightlyDir) sortedYears
  where
    processYear dir yearStr = do
      case reads yearStr of
        [(year, "")] -> do
          let yearPath = dir </> yearStr
          monthDirs <- listDirectory yearPath
          let sortedMonths = sortOn (\s -> read s :: Int) $ filter (all (`elem` ("0123456789" :: String))) monthDirs
          months <- mapM (processMonth yearPath year) sortedMonths
          return $ concat months
        _ -> return []
    processMonth dir year monthStr = do
      case reads monthStr of
        [(month, "")] -> do
          let monthPath = dir </> monthStr
          dayFiles <- listDirectory monthPath
          let sortedDays = sortOn (\s -> read (take (length s - 5) s) :: Int) $
                           filter (\s -> takeExtension s == ".yaml") dayFiles
          mapM (processDay monthPath year month) sortedDays
        _ -> return []
    processDay dir year month fname
      | takeExtension fname == ".yaml" = do
          case reads $ take (length fname - 5) fname of
            [(day, "")] -> do
              ghc <- extractGHCVersion (dir </> fname)
              return (NightlyVersion year month day, ghc)
            _ -> error $ "Invalid day in " ++ fname
      | otherwise = error $ "Unexpected file in nightly directory: " ++ fname ++
                           " (expected .yaml extension)"

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
              parseCompiler compiler
            _ -> error $ "No compiler field in resolver in " ++ file
        _ ->
          -- Try old format (compiler at top level)
          case Yaml.parseMaybe (.: "compiler") obj of
            Just (Aeson.String compiler) ->
              parseCompiler compiler
            _ -> error $ "No compiler field in " ++ file
    _ -> error $ "Invalid YAML in " ++ file
  where
    parseCompiler compiler =
      case parseGHCVersionText (T.drop 4 compiler) of
        Just ghc -> return ghc
        Nothing -> error $ "Invalid GHC version format in " ++ file ++ ": " ++ T.unpack compiler

-- | Write LTS CSV file
writeLTSCSV :: FilePath -> [(LTSVersion, GHCVersion)] -> IO ()
writeLTSCSV path entries = do
  let sorted = sortBy (comparing fst) entries
  let lines = map formatLTSEntry sorted
  TIO.writeFile path $ T.unlines lines
  where
    formatLTSEntry (LTSVersion maj min, ghc) =
      T.pack (show maj) <> "." <> T.pack (show min) <> "," <> formatGHCVersion ghc

-- | Write nightly CSV file
writeNightlyCSV :: FilePath -> [(NightlyVersion, GHCVersion)] -> IO ()
writeNightlyCSV path entries = do
  let sorted = sortBy (comparing fst) entries
  let lines = map formatNightlyEntry sorted
  TIO.writeFile path $ T.unlines lines
  where
    formatNightlyEntry (nightly, ghc) =
      formatNightlyVersion nightly <> "," <> formatGHCVersion ghc

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
    formatGHCEntry (ghc, LTS (LTSVersion maj min)) =
      formatGHCVersion ghc <> ",lts-" <> T.pack (show maj) <> "." <> T.pack (show min)
    formatGHCEntry (ghc, Nightly nightly) =
      formatGHCVersion ghc <> ",nightly-" <> formatNightlyVersion nightly

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
        [ver, ghcText] ->
          case T.splitOn "." ver of
            [majStr, minStr] ->
              case (reads $ T.unpack majStr, reads $ T.unpack minStr, parseGHCVersionText ghcText) of
                ([(maj, "")], [(min, "")], Just ghc) ->
                  [(LTSVersion maj min, ghc)]
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
        [dateText, ghcText] ->
          case (parseNightlyVersionText dateText, parseGHCVersionText ghcText) of
            (Just nightly, Just ghc) -> [(nightly, ghc)]
            _ -> []
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
        [ghcText, snap] ->
          case (parseGHCVersionText ghcText, parseSnapshot snap) of
            (Just ghc, Just s) -> [(ghc, s)]
            _ -> []
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
          case parseNightlyVersionText (T.drop 8 snap) of
            Just nightly -> Just $ Nightly nightly
            Nothing -> Nothing
      | otherwise = Nothing

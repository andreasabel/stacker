{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( runCommand
  ) where

import Control.Monad (forM_, when)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.ANSI
import System.Directory (makeAbsolute, doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Text.Printf (printf)
import Types
import Config
import CSV
import Git
import Analysis
import StackYaml
import ColorOption
import License (licenseText)
import qualified XDG

-- | Run a command
runCommand :: Options -> IO ()
runCommand opts = do
  useColor <- shouldUseColor (optColor opts)
  case optCommand opts of
    Version -> printVersion
    NumericVersion -> putStrLn appVersion
    PrintLicense -> putStrLn licenseText
    Help -> printHelp
    Config configCmd -> runConfig configCmd  -- Handle config first!
    cmd -> runEssentialCommand useColor cmd

-- | Run an essential command (requires repo setup)
runEssentialCommand :: Bool -> Command -> IO ()
runEssentialCommand useColor cmd = do
  repoPath <- getRepoPath
  ensureRepo repoPath
  
  -- Ensure CSV files exist
  stateDir <- XDG.getStateDir
  csvExists <- doesFileExist (stateDir </> "ghc.csv")
  when (not csvExists) $ generateCSVs repoPath
  
  case cmd of
    DryRun -> runDryRun useColor
    Bump -> runBump
    Update -> runUpdate repoPath
    Info -> runInfo repoPath
    _ -> return ()

-- | Print version information
printVersion :: IO ()
printVersion = do
  putStrLn $ appName ++ " version " ++ appVersion
  putStrLn copyright

-- | Print help
printHelp :: IO ()
printHelp = do
  printVersion
  putStrLn ""
  putStrLn "A tool to bump snapshots (resolvers) in stack*.yaml files."
  putStrLn ""
  putStrLn "Usage: stack-snapshots [COMMAND] [OPTIONS]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  bump           Update stack*.yaml files"
  putStrLn "  dry-run        Show what would be updated (default)"
  putStrLn "  config         Configure stack-snapshots"
  putStrLn "  update         Update stackage snapshots database"
  putStrLn "  info           Print GHC version to snapshot mapping"
  putStrLn "  version        Print version information"
  putStrLn "  license        Print license text"
  putStrLn "  help           Print this help"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --color WHEN   Use colored output (always, never, auto)"

-- | Run dry-run command
runDryRun :: Bool -> IO ()
runDryRun useColor = do
  db <- loadSnapshotDB
  actions <- analyzeAllStackYamls db
  
  -- Sort actions by filename
  let sortedActions = sortBy (comparing actionFile) actions
  
  forM_ sortedActions $ \action -> do
    printAction useColor action

-- | Print an action
printAction :: Bool -> Action -> IO ()
printAction useColor action = do
  let file = actionFile action
  let oldSnap = actionOldSnapshot action
  let newSnap = actionNewSnapshot action
  
  -- Print with proper alignment (file padded to 20 chars, oldSnap to 25 chars)
  when useColor $ setSGR [SetConsoleIntensity BoldIntensity]
  putStr $ padRight 20 file
  when useColor $ setSGR [Reset]
  
  putStr $ padRight 25 (T.unpack oldSnap)
  
  case newSnap of
    Nothing -> do
      when useColor $ setSGR [SetColor Foreground Vivid Green]
      putStr "✓ up to date"
      when useColor $ setSGR [Reset]
    Just new -> do
      when useColor $ setSGR [SetColor Foreground Vivid Yellow]
      putStr "→ bump to "
      putStr $ T.unpack new
      when useColor $ setSGR [Reset]
  
  putStrLn ""

-- | Pad string to the right
padRight :: Int -> String -> String
padRight n s = take n (s ++ repeat ' ')

-- | Run bump command
runBump :: IO ()
runBump = do
  db <- loadSnapshotDB
  actions <- analyzeAllStackYamls db
  
  forM_ actions $ \action -> do
    case actionNewSnapshot action of
      Nothing -> return ()
      Just _ -> do
        putStrLn $ "Updating " ++ actionFile action
        applyAction action

-- | Run update command
runUpdate :: FilePath -> IO ()
runUpdate repoPath = do
  putStrLn $ "Repository: " ++ repoPath
  updateRepo repoPath
  generateCSVs repoPath

-- | Run info command
runInfo :: FilePath -> IO ()
runInfo repoPath = do
  putStrLn $ "repo: " ++ repoPath
  db <- loadSnapshotDB
  putStrLn "snapshots:"
  
  let ghcEntries = Map.toAscList (dbGHC db)
  forM_ ghcEntries $ \(ghc, snapshot) -> do
    putStrLn $ "  " ++ formatGHCVersionText ghc ++ ": " ++ T.unpack (formatSnapshotText snapshot)

-- | Format GHC version as text
formatGHCVersionText :: GHCVersion -> String
formatGHCVersionText (GHCVersion maj1 maj2 minV) =
  show maj1 ++ "." ++ show maj2 ++ "." ++ show minV

-- | Format snapshot as text
formatSnapshotText :: Snapshot -> Text
formatSnapshotText (LTS (LTSVersion maj min)) =
  "lts-" <> T.pack (show maj) <> "." <> T.pack (show min)
formatSnapshotText (Nightly (NightlyVersion year month day)) =
  T.pack $ "nightly-" ++ printf "%d-%02d-%02d" year month day

-- | Run config command
runConfig :: ConfigCmd -> IO ()
runConfig (SetRepo path) = do
  absPath <- makeAbsolute path
  saveConfig $ AppConfig (Just absPath)
  putStrLn $ "Repository path set to: " ++ absPath
  configFile <- XDG.getConfigFile
  putStrLn $ "Configuration saved to: " ++ configFile

{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( runCommand
  ) where

import Control.Monad (forM_, when)
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import System.Console.ANSI
import System.Directory (makeAbsolute)
import Text.Printf (printf)
import Types
import Config
import CSV
import Git
import Analysis
import StackYaml
import ColorOption
import License (licenseText)
import XDG qualified

-- | Run a command
runCommand :: Options -> IO ()
runCommand opts = do
  useColor <- shouldUseColor (optColor opts)
  case optCommand opts of
    Version -> printVersion
    NumericVersion -> putStrLn appVersion
    PrintLicense -> putStrLn licenseText
    Help -> do
      -- Manually render help text
      -- While not ideal, this ensures help works. The structure mirrors optparse-applicative's output.
      putStrLn $ "stacker version " ++ appVersion
      putStrLn ""
      putStrLn "Usage: stacker [COMMAND | (-V|--version) | --numeric-version |"
      putStrLn "                --license | (-h|--help)] [--color WHEN]"
      putStrLn ""
      putStrLn "  A tool to bump snapshots (resolvers) in stack*.yaml files"
      putStrLn ""
      putStrLn "Available commands:"
      putStrLn "  bump [FILES...]          Update FILES (or all stack*.yaml files in current directory if none specified)"
      putStrLn "  dry-run [FILES...]       Show what would be updated by 'bump' (default)"
      putStrLn "  update                   Update stackage snapshots database"
      putStrLn "  info                     Print GHC version to snapshot mapping"
      putStrLn "  config                   Configure stacker"
      putStrLn ""
      putStrLn "Available options:"
      putStrLn "  --color WHEN             Use colored output (always, never, auto)"
      putStrLn ""
      putStrLn "Inessential commands:"
      putStrLn "  version                  Print version information (also: -V, --version)"
      putStrLn "  numeric-version          Print version number (also: --numeric-version)"
      putStrLn "  license                  Print license text (also: --license)"
      putStrLn "  help                     Print this help (also: -h, --help)"
      putStrLn ""
      putStrLn "Option form of inessential commands:"
      putStrLn "  -V,--version             Print version information"
      putStrLn "  --numeric-version        Print version number"
      putStrLn "  --license                Print license text"
      putStrLn "  -h,--help                Print help"
      putStrLn ""
      putStrLn "For more information, see the README"
    Config configCmd -> runConfig configCmd  -- Handle config first!
    cmd -> runEssentialCommand useColor cmd

-- | Run an essential command (requires CSV files)
runEssentialCommand :: Bool -> Command -> IO ()
runEssentialCommand useColor cmd = do
  -- Ensure CSV files exist (copy from data dir if needed)
  ensureCSVFiles

  case cmd of
    DryRun files -> runDryRun useColor files
    Bump files -> runBump files
    Update -> do
      -- Only for Update command, ensure repo exists and update it
      repoPath <- getRepoPath
      ensureRepo repoPath
      runUpdate repoPath
    Info -> do
      -- Info command shows the repo path
      repoPath <- getRepoPath
      runInfo repoPath
    _ -> return ()

-- | Print version information
printVersion :: IO ()
printVersion = do
  putStrLn $ appName ++ " version " ++ appVersion
  putStrLn copyright

-- | Print text with optional color formatting
withColor :: Bool -> [SGR] -> IO () -> IO ()
withColor useColor sgr action = do
  when useColor $ setSGR sgr
  action
  when useColor $ setSGR [Reset]

-- | Run dry-run command
runDryRun :: Bool -> [FilePath] -> IO ()
runDryRun useColor files = do
  db <- loadSnapshotDB
  actions <- analyzeStackYamls db files

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
  let symlinkTarget = actionSymlinkTarget action

  -- Print with proper alignment (file padded to 20 chars, oldSnap to 25 chars)
  withColor useColor [SetConsoleIntensity BoldIntensity] $
    putStr $ padRight 20 file

  putStr $ padRight 25 (T.unpack oldSnap)

  case symlinkTarget of
    Just target -> do
      -- This is a symlink to another stack*.yaml file
      withColor useColor [SetColor Foreground Dull White] $ do
        putStr "= symlink to "
        putStr target
    Nothing ->
      case newSnap of
        Nothing -> do
          withColor useColor [SetColor Foreground Vivid Green] $
            putStr "✓ up to date"
        Just new -> do
          withColor useColor [SetColor Foreground Vivid Yellow] $ do
            putStr "→ bump to "
            putStr $ T.unpack new

  putStrLn ""

-- | Pad string to the right
padRight :: Int -> String -> String
padRight n s = take n (s ++ repeat ' ')

-- | Run bump command
runBump :: [FilePath] -> IO ()
runBump files = do
  db <- loadSnapshotDB
  actions <- analyzeStackYamls db files
  mapM_ (applyAction True) actions

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
formatSnapshotText (LTS (LTSVersion major minor)) =
  "lts-" <> T.pack (show major) <> "." <> T.pack (show minor)
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

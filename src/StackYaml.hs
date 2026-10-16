{-# LANGUAGE OverloadedStrings #-}

module StackYaml
  ( findStackYamlFiles
  , findStackYamlFilesRecursive
  , parseStackYaml
  , findSnapshot
  , applyAction
  , isStackYaml
  , getSymlinkMap
  ) where

import Control.Monad (filterM, when)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory, doesFileExist, pathIsSymbolicLink, getSymbolicLinkTarget, doesDirectoryExist)
import PathUtil ((</>), normalizeFilePath, collapseDots, takeDirectory, takeFileName, toForwardSlashes)
import Types (Action(..), Bump(..))

-- | Check if a filename is a stack*.yaml file
isStackYaml :: FilePath -> Bool
isStackYaml name =
  let fname = takeFileName name
  in "stack" `isPrefixOf` fname && ".yaml" `isSuffixOf` fname

-- | Find all stack*.yaml files in the current directory
findStackYamlFiles :: IO [FilePath]
findStackYamlFiles = do
  files <- listDirectory "."
  let candidates = filter isStackYaml files
  sort <$> filterM doesFileExist candidates

-- | Find all stack*.yaml files recursively in all subdirectories
findStackYamlFilesRecursive :: IO [FilePath]
findStackYamlFilesRecursive = findStackYamlFilesInDir "."

-- | Helper function to find stack*.yaml files recursively in a directory
findStackYamlFilesInDir :: FilePath -> IO [FilePath]
findStackYamlFilesInDir dir = do
  entries <- listDirectory dir
  -- Use custom </> to ensure consistent path output with "/" separator on all platforms
  let fullPaths = map (dir </>) entries

  -- Process files and directories separately
  files <- filterM doesFileExist fullPaths
  dirs <- filterM doesDirectoryExist fullPaths

  -- Find stack*.yaml files in current directory, normalize to remove "./" prefix
  let stackYamls = map normalizeFilePath $ filter isStackYaml files

  -- Recursively search subdirectories
  subResults <- mapM findStackYamlFilesInDir dirs

  -- Combine and sort all results (all paths are already normalized)
  return $ sort (stackYamls ++ concat subResults)

-- | Get a map of symlinks to their targets (only for symlinks pointing to other stack*.yaml files in the list)
getSymlinkMap :: [FilePath] -> IO (Map.Map FilePath FilePath)
getSymlinkMap files = do
  -- Find symlinks in the list
  symlinks <- filterM pathIsSymbolicLink files

  -- For each symlink, check if it points to another file in the list
  results <- mapM checkSymlink symlinks

  return $ Map.fromList $ catMaybes results
  where
    checkSymlink :: FilePath -> IO (Maybe (FilePath, FilePath))
    checkSymlink link = do
      target <- getSymbolicLinkTarget link
      -- Convert backslashes to forward slashes (Windows compatibility)
      let normalizedTarget = toForwardSlashes target
      -- Resolve the target relative to the symlink's directory
      let symlinkDir = takeDirectory link
      let targetPath = symlinkDir </> normalizedTarget
      -- Normalize and collapse .. components using our custom functions
      let resolvedTarget = normalizeFilePath $ collapseDots targetPath
      -- Check if the target is in our file list
      if resolvedTarget `elem` files
        then return $ Just (link, resolvedTarget)
        else return Nothing

-- | Parse a stack.yaml file to extract the snapshot field
parseStackYaml :: FilePath -> IO (Maybe (Text, Bool, (Int, Int)))
parseStackYaml file = findSnapshot <$> TIO.readFile file

-- | Find the snapshot (or, failing that, resolver) field in the contents
--   of a stack.yaml file.
--   Returns the value, whether it is a @resolver@ field,
--   and the character span of the value.
--
--   Only top-level keys are considered, i.e., the field name has to start
--   at the beginning of a line.  Thus, commented-out fields like
--   @# snapshot: lts-22.0@ and nested keys are ignored.
--   A trailing comment is not part of the value.
findSnapshot :: Text -> Maybe (Text, Bool, (Int, Int))
findSnapshot content =
  case findField "snapshot:" of
    Just (value, start, end) -> Just (T.pack value, False, (start, end))
    Nothing ->
      case findField "resolver:" of
        Just (value, start, end) -> Just (T.pack value, True, (start, end))
        Nothing -> Nothing
  where
    s = T.unpack content

    findField :: String -> Maybe (String, Int, Int)
    findField field = go s 0
      where
        -- Invariant: @l@ is the rest of the input, starting at the beginning
        -- of a line at character offset @pos@.
        go [] _ = Nothing
        go l pos
          | field `isPrefixOf` l
          , let afterField = drop (length field) l
                trimmed = dropWhile isBlank afterField
                value = stripComment $ takeWhile (`notElem` ("\n\r" :: String)) trimmed
          , not (null value)
          = let valueStart = pos + length field + (length afterField - length trimmed)
            in  Just (value, valueStart, valueStart + length value)
          | otherwise =
              let (line, rest) = break (== '\n') l
              in  case rest of
                    []      -> Nothing
                    _ : l'  -> go l' (pos + length line + 1)

    isBlank :: Char -> Bool
    isBlank c = c == ' ' || c == '\t'

    -- Remove a trailing comment.  In YAML, a comment starts with @#@
    -- at the beginning or after whitespace.
    stripComment :: String -> String
    stripComment ('#' : _) = []
    stripComment v         = dropWhileEnd isBlank $ cut v
      where
        cut (c : '#' : _) | isBlank c = []
        cut (c : cs) = c : cut cs
        cut [] = []

-- | Apply an action to update a stack.yaml file
applyAction :: Bool -> Action -> IO ()
applyAction verbose action = do
  -- Skip symlinks that point to other stack*.yaml files in the list
  case actionSymlinkTarget action of
    Just _ -> return ()  -- Skip symlinks
    Nothing ->
      case actionBump action of
        UpToDate -> return ()
        Unknown -> return ()
        BumpTo newSnap -> do
          when verbose $ putStrLn $ "Updating " ++ actionFile action
          content <- TIO.readFile (actionFile action)
          let (before, after) = splitAtSpan (actionSpan action) content
          let updated = before <> newSnap <> after
          TIO.writeFile (actionFile action) updated

-- | Split text at a character span
splitAtSpan :: (Int, Int) -> Text -> (Text, Text)
splitAtSpan (start, end) text =
  let before = T.take start text
      after = T.drop end text
  in (before, after)

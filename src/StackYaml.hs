{-# LANGUAGE OverloadedStrings #-}

module StackYaml
  ( findStackYamlFiles
  , findStackYamlFilesRecursive
  , parseStackYaml
  , applyAction
  , isStackYaml
  , getSymlinkMap
  ) where

import Control.Monad (filterM, when)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory, doesFileExist, pathIsSymbolicLink, getSymbolicLinkTarget, doesDirectoryExist)
import System.FilePath (takeFileName, takeDirectory, normalise, makeRelative, splitDirectories, joinPath)
import PathUtil ((</>), normalizeFilePath)
import Types (Action(..))

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
  -- Use custom </> to ensure consistent paths across all OSs
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
      -- Resolve the target relative to the symlink's directory
      let symlinkDir = takeDirectory link
      let targetPath = symlinkDir </> target
      -- Normalize and collapse .. components
      let normalizedTarget = collapseDotDots $ normalise targetPath
      let relativeTarget = normalizeFilePath $ makeRelative "." normalizedTarget
      -- Check if the target is in our file list
      if relativeTarget `elem` files
        then return $ Just (link, relativeTarget)
        else return Nothing
    
    -- Collapse .. components in a path
    collapseDotDots :: FilePath -> FilePath
    collapseDotDots path =
      let dirs = splitDirectories path
          collapsed = foldl collapseDir [] dirs
      in joinPath collapsed
    
    collapseDir :: [String] -> String -> [String]
    collapseDir acc "." = acc
    collapseDir (dir:rest) ".." | dir /= ".." = rest
    collapseDir acc dir = acc ++ [dir]

-- | Parse a stack.yaml file to extract the snapshot field
parseStackYaml :: FilePath -> IO (Maybe (Text, Bool, (Int, Int)))
parseStackYaml file = do
  content <- TIO.readFile file
  return $ findSnapshot (T.unpack content) 0
  where
    findSnapshot s pos =
      case findField "snapshot:" s pos of
        Just (value, start, end) -> Just (T.pack value, False, (start, end))
        Nothing ->
          case findField "resolver:" s pos of
            Just (value, start, end) -> Just (T.pack value, True, (start, end))
            Nothing -> Nothing

    findField :: String -> String -> Int -> Maybe (String, Int, Int)
    findField field s pos =
      findFieldHelper field s pos s

    findFieldHelper :: String -> String -> Int -> String -> Maybe (String, Int, Int)
    findFieldHelper _field _orig _pos [] = Nothing
    findFieldHelper field orig pos s@(_:cs)
      | field `isPrefixOf` s =
          let afterField = drop (length field) s
              trimmed = dropWhile (\c -> c `elem` (" \t" :: String)) afterField
              value = takeWhile (`notElem` ("\n\r" :: String)) trimmed
              valueStart = pos + (length orig - length s) + length field + (length afterField - length trimmed)
              valueEnd = valueStart + length value
          in if null value
               then Nothing
               else Just (value, valueStart, valueEnd)
      | otherwise = findFieldHelper field orig pos cs

-- | Apply an action to update a stack.yaml file
applyAction :: Bool -> Action -> IO ()
applyAction verbose action = do
  -- Skip symlinks that point to other stack*.yaml files in the list
  case actionSymlinkTarget action of
    Just _ -> return ()  -- Skip symlinks
    Nothing ->
      case actionNewSnapshot action of
        Nothing -> return ()  -- No update needed
        Just newSnap -> do
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

-- | Path utilities for cross-platform compatibility
-- 
-- This module provides path utilities that use forward slash (/) as the 
-- path separator on all platforms, including Windows. While Windows primarily
-- uses backslash (\), it also accepts forward slash for relative paths,
-- which is what this tool deals with.
--
-- This approach ensures consistent path handling and output across all platforms,
-- avoiding the path separator issues that can occur when using System.FilePath's
-- platform-specific path combinators.
module PathUtil
  ( (</>)
  , normalizeFilePath
  , splitPath
  , joinPath
  , collapseDots
  , takeDirectory
  , takeFileName
  ) where

import Data.List (stripPrefix)

-- | Cross-platform path combination using forward slash
-- This ensures consistent behavior across Linux, macOS, and Windows
-- for relative paths (which is what this tool processes).
--
-- Note: This is specifically designed for combining relative path components.
-- For absolute paths or complex path manipulations, System.FilePath should be used.
(</>) :: FilePath -> FilePath -> FilePath
"" </> b = b
a </> "" = a
a </> b = a ++ "/" ++ b

-- | Normalize a file path by removing "./" prefix
-- This is used to ensure consistent output format where paths
-- don't have a leading "./" component.
normalizeFilePath :: FilePath -> FilePath
normalizeFilePath path =
  case stripPrefix "./" path of
    Just rest -> rest
    Nothing -> path

-- | Split a path into components using forward slash as separator
-- Works consistently across all platforms
splitPath :: FilePath -> [String]
splitPath "" = []
splitPath path = split path
  where
    split "" = []
    split s = case break (== '/') s of
      (comp, "") -> [comp]
      (comp, _:rest) -> comp : split rest

-- | Join path components using forward slash
-- Works consistently across all platforms
joinPath :: [String] -> FilePath
joinPath [] = ""
joinPath [x] = x
joinPath (x:xs) = x ++ "/" ++ joinPath xs

-- | Collapse ".." and "." components in a path
-- This normalizes paths like "a/b/../c" to "a/c"
collapseDots :: FilePath -> FilePath
collapseDots path = joinPath $ reverse $ foldl collapseDir [] (splitPath path)
  where
    collapseDir :: [String] -> String -> [String]
    collapseDir acc "." = acc
    collapseDir (prev:rest) ".." | prev /= ".." = rest
    collapseDir acc ".." = ".." : acc
    collapseDir acc dir = dir : acc

-- | Get the directory part of a file path
-- Uses forward slash as separator on all platforms
-- Examples: takeDirectory "a/b/c" = "a/b", takeDirectory "file.txt" = "."
takeDirectory :: FilePath -> FilePath
takeDirectory path =
  case reverse (splitPath path) of
    [] -> "."
    [_] -> "."
    (_:dirs) -> joinPath (reverse dirs)

-- | Get the file name part of a file path
-- Uses forward slash as separator on all platforms
-- Examples: takeFileName "a/b/c.txt" = "c.txt", takeFileName "file.txt" = "file.txt"
takeFileName :: FilePath -> FilePath
takeFileName path =
  case reverse (splitPath path) of
    [] -> ""
    (name:_) -> name

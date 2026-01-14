-- | Path utilities for cross-platform compatibility
module PathUtil
  ( (</>)
  , normalizeFilePath
  ) where

import Data.List (stripPrefix)

-- | Cross-platform path combination using forward slash
-- This ensures consistent behavior across Linux, macOS, and Windows
(</>) :: FilePath -> FilePath -> FilePath
"" </> b = b
a </> "" = a
a </> b = a ++ "/" ++ b

-- | Normalize a file path by removing "./" prefix
normalizeFilePath :: FilePath -> FilePath
normalizeFilePath path =
  case stripPrefix "./" path of
    Just rest -> rest
    Nothing -> path

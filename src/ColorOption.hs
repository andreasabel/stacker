{-# LANGUAGE OverloadedStrings #-}

module ColorOption
  ( ColorWhen(..)
  , shouldUseColor
  ) where

import Types (ColorWhen(..))
import System.IO (hIsTerminalDevice, stdout)
import System.Environment (lookupEnv)

-- | Determine if color should be used based on the color option
shouldUseColor :: ColorWhen -> IO Bool
shouldUseColor Always = return True
shouldUseColor Never = return False
shouldUseColor Auto = do
  -- Respect NO_COLOR environment variable per https://no-color.org
  noColor <- lookupEnv "NO_COLOR"
  case noColor of
    Just _ -> return False  -- NO_COLOR is set, disable colors
    Nothing -> hIsTerminalDevice stdout  -- Check if stdout is a terminal

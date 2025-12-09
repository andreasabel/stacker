{-# LANGUAGE OverloadedStrings #-}

module ColorOption 
  ( ColorWhen(..)
  , shouldUseColor
  ) where

import Types (ColorWhen(..))
import System.IO (hIsTerminalDevice, stdout)

-- | Determine if color should be used based on the color option
shouldUseColor :: ColorWhen -> IO Bool
shouldUseColor Always = return True
shouldUseColor Never = return False
shouldUseColor Auto = hIsTerminalDevice stdout

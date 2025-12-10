{-# LANGUAGE TemplateHaskell #-}

module License (licenseText) where

import Data.FileEmbed (embedStringFile)

-- | BSD-3-Clause license text embedded from LICENSE file
licenseText :: String
licenseText = $(embedStringFile "LICENSE")

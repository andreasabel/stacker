module Main where

import Options (parseOptions)
import Commands (runCommand)

main :: IO ()
main = do
  opts <- parseOptions
  runCommand opts

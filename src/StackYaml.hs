{-# LANGUAGE OverloadedStrings #-}

module StackYaml
  ( findStackYamlFiles
  , parseStackYaml
  , applyAction
  ) where

import Control.Monad (filterM)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, doesFileExist)
import System.FilePath (takeFileName)
import Types (Action(..))

-- | Find all stack*.yaml files in the current directory
findStackYamlFiles :: IO [FilePath]
findStackYamlFiles = do
  files <- listDirectory "."
  let candidates = filter isStackYaml files
  sort <$> filterM doesFileExist candidates
  where
    isStackYaml name =
      let fname = takeFileName name
      in "stack" `isPrefixOf` fname && ".yaml" `isSuffixOf` fname

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
applyAction :: Action -> IO ()
applyAction action = do
  case actionNewSnapshot action of
    Nothing -> return ()  -- No update needed
    Just newSnap -> do
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

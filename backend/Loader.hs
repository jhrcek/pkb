{-# LANGUAGE OverloadedStrings #-}

module Loader (loadNotes) where

import Control.Monad (zipWithM)
import Data.Note
import Data.Text.IO as T
import Data.Text.Lazy (fromStrict, pack, replace)
import Data.Text.Lazy.IO as LT
import System.Directory (listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension, takeFileName)

loadNotes :: FilePath -> IO [Note]
loadNotes notesDir = do
  files <- listDirectory notesDir
  let notes = map (notesDir </>) $ filter (\f -> takeExtension f == ".md") files
  LT.putStrLn $ "Loaded " <> pack (show $ length notes) <> " notes from " <> pack notesDir
  zipWithM filePathToNote [1 ..] notes

filePathToNote :: Int -> FilePath -> IO Note
filePathToNote nid filePath = do
  body <- fromStrict <$> T.readFile filePath
  pure (Note nid fileName title body)
  where
    title = replace "_" " " . pack $ takeBaseName fileName
    fileName = takeFileName filePath

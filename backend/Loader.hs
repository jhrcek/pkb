{-# LANGUAGE OverloadedStrings #-}
module Loader (loadNotes) where

import Control.Monad (zipWithM)
import Data.Text.IO as T
import Data.Text.Lazy.IO as LT
import Data.Text.Lazy (fromStrict, pack, replace)
import Notes
import System.Directory (listDirectory)
import System.FilePath
import Data.Monoid ((<>))

--TODO make this configurable
notesDir :: FilePath
notesDir = "/home/hrk/Tmp/pkbDev"

--TODO dubious combination of strict text (I want all notes to be loaded eagerly on app initialization)
-- and lazy text - investigate if there's a better way
loadNotes :: IO [Note]
loadNotes = do
  files <- listDirectory notesDir
  let notes = map (notesDir </>) $ filter (\f -> takeExtension f == ".md") files
  LT.putStrLn $ "Loaded " <> pack (show $ length notes) <> " from " <> pack notesDir
  zipWithM fileToNote [1..] notes

fileToNote :: Int -> FilePath -> IO Note
fileToNote id file = (Note id title . fromStrict) <$> T.readFile file
  where title = replace "_" " " . pack $ takeBaseName file

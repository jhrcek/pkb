{-# LANGUAGE OverloadedStrings #-}
module Loader (loadNotes) where

import Control.Monad (zipWithM)
import Data.Monoid ((<>))
import Data.Text.IO as T
import Data.Text.Lazy (fromStrict, pack, replace)
import Data.Text.Lazy.IO as LT
import System.Directory (listDirectory)
import System.FilePath

import Data.Note

--TODO dubious combination of strict text (I want all notes to be loaded eagerly on app initialization)
-- and lazy text - investigate if there's a better way
loadNotes :: FilePath -> IO [Note]
loadNotes notesDir = do
    files <- listDirectory notesDir
    let notes = map (notesDir </>) $ filter (\f -> takeExtension f == ".md") files
    LT.putStrLn $ "Loaded " <> pack (show $ length notes) <> " from " <> pack notesDir
    zipWithM fileToNote [1..] notes

fileToNote :: Int -> FilePath -> IO Note
fileToNote nid file = (Note nid title . fromStrict) <$> T.readFile file
  where
    title = replace "_" " " . pack $ takeBaseName file

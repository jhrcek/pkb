{-# LANGUAGE OverloadedStrings #-}

module Handler.Notes (apiHandler) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, lift)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import System.FilePath ((</>))
import Web.Scotty.Trans (get, json, jsonData, post)

import qualified Config
import Data.Note (Note (Note))
import qualified Loader
import Types (ActionC, ScottyC)

apiHandler :: ScottyC ()
apiHandler = do
    getNotes
    postNote

getNotes :: ScottyC ()
getNotes = get "/notes" $ do
    notesDir <- getNotesDir
    notes <- liftIO $ Loader.loadNotes notesDir
    json notes

postNote :: ScottyC ()
postNote = post "/notes" $ do
    note <- jsonData :: ActionC Note
    notesDir <- getNotesDir
    createNote notesDir note

createNote :: FilePath -> Note -> ActionC ()
createNote notesDir (Note _id title body) =
    liftIO $ Text.writeFile filePath body
  where
    fileName = Text.unpack $ Text.replace " " "_" title <> ".md"
    filePath = notesDir </> fileName

getNotesDir :: ActionC FilePath
getNotesDir = lift (asks Config.notesDir)

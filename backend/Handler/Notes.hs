{-# LANGUAGE OverloadedStrings #-}

module Handler.Notes (apiHandler) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, lift)
import qualified Data.Text.Lazy.IO as Text
import System.FilePath ((</>))
import Web.Scotty.Trans (get, json, jsonData, post, redirect)

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
    writeNote notesDir note
    redirect "/notes"

writeNote :: FilePath -> Note -> ActionC ()
writeNote notesDir (Note _id noteFile _title body) =
    liftIO $ Text.writeFile notePath body
  where
    notePath = notesDir </> noteFile

getNotesDir :: ActionC FilePath
getNotesDir = lift (asks Config.notesDir)

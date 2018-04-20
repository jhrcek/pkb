{-# LANGUAGE OverloadedStrings #-}

module Handler.Notes (apiHandler) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, lift)
import qualified Loader
import Web.Scotty.Trans (get, json)

import qualified Config
import Types (ScottyC)

apiHandler :: ScottyC ()
apiHandler = getNotes

getNotes :: ScottyC ()
getNotes = get "/notes" $ do
    notesDir <- lift $ asks Config.notesDir
    notes <- liftIO $ Loader.loadNotes notesDir
    json notes

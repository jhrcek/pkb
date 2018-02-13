{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Config
import Data.Default.Class (def)
import qualified Loader
import Web.Scotty (Options, file, get, html, json, notFound, scottyOpts,
                   verbose)


main :: IO ()
main = do
    config <- Config.parseConfig
    notes <- Loader.loadNotes (Config.notesDir config)
    putStrLn "Listening on http://localhost:3000"
    scottyOpts beQuiet $ do
        get "/" $ file "index.html"
        get "/script.js" $ file "script.js"
        get "/style.css" $ file "style.css"
        get "/notes" $ json notes
        notFound $ html "There is nothing here, pal!"


beQuiet :: Options
beQuiet = def {verbose = 0}

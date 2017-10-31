{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config
import Loader
import Web.Scotty

main :: IO ()
main = do
  config <- Config.parseConfig
  notes <- Loader.loadNotes (notesDir config)
  scotty 3000 $ do
    get "/" $ file "index.html"
    get "/script.js" $ file "script.js"
    get "/style.css" $ file "style.css"
    get "/notes" $ json notes
    notFound $ html "<h1>Not found!</h1>"

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Data.Monoid
import Data.Text.Lazy (pack)
import Loader
import Notes
import System.Directory (listDirectory)
import Web.Scotty

main :: IO ()
main = do
  notes <- Loader.loadNotes
  scotty 3000 $ do
    get "/" $ file "index.html"
    get "/script.js" $ file "script.js"
    get "/style.css" $ file "style.css"
    get "/notes" $ json notes
    notFound $ html "<h1>Not found!</h1>"

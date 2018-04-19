{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Config
import Data.Default.Class (def)
import qualified Loader
import Web.Scotty (Options, get, html, json, notFound, scottyOpts, verbose)

import qualified Pkb.Static as Static

main :: IO ()
main = do
    config <- Config.parseConfig
    notes <- Loader.loadNotes (Config.notesDir config)
    putStrLn "Listening on http://localhost:3000"
    scottyOpts beQuiet $ do
        Static.staticAssets
        get "/notes" $ json notes
        notFound $ html "There is nothing here, pal!"

beQuiet :: Options
beQuiet = def {verbose = 0}

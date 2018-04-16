{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import qualified Config
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Default.Class (def)
import Data.FileEmbed (embedFile)
import Data.Text.Lazy (Text)
import qualified Loader
import Web.Scotty (ActionM, Options, ScottyM, get, html, json, notFound, raw,
                   scottyOpts, setHeader, verbose)

main :: IO ()
main = do
    config <- Config.parseConfig
    notes <- Loader.loadNotes (Config.notesDir config)
    putStrLn "Listening on http://localhost:3000"
    scottyOpts beQuiet $ do
        staticAssets
        get "/notes" $ json notes
        notFound $ html "There is nothing here, pal!"

beQuiet :: Options
beQuiet = def {verbose = 0}

staticAssets :: ScottyM ()
staticAssets = do
    indexHtml
    scriptJs
    styleCss

indexHtml :: ScottyM ()
indexHtml = get "/" $ do
    setContentType "text/html;charset=UTF-8"
    rawLazy $(embedFile "static/index.html")

scriptJs :: ScottyM ()
scriptJs = get "/script.js" $ do
    setContentType "text/javascript;charset=UTF-8"
    rawLazy $(embedFile "static/script.js")

styleCss :: ScottyM ()
styleCss = get "/style.css" $ do
    setContentType "text/css;charset=UTF-8"
    rawLazy $(embedFile "static/style.css")

setContentType :: Text -> ActionM ()
setContentType = setHeader "Content-Type"

rawLazy :: ByteString -> ActionM ()
rawLazy = raw . fromStrict

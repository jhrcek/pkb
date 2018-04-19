{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Pkb.Static (staticAssets) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed (embedFile)
import Data.Text.Lazy (Text)
import Web.Scotty (ActionM, ScottyM, get, raw, setHeader)

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

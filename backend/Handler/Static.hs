{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Static (assetHandler) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed (embedFile)
import Data.Text.Lazy (Text)
import Web.Scotty.Trans (get, raw, setHeader)

import Types (ActionC, ScottyC)

assetHandler :: ScottyC ()
assetHandler = do
    indexHtml
    scriptJs
    styleCss

indexHtml :: ScottyC ()
indexHtml = get "/" $ do
    setContentType "text/html;charset=UTF-8"
    rawLazy $(embedFile "static/index.html")

scriptJs :: ScottyC ()
scriptJs = get "/script.js" $ do
    setContentType "text/javascript;charset=UTF-8"
    rawLazy $(embedFile "static/script.js")

styleCss :: ScottyC ()
styleCss = get "/style.css" $ do
    setContentType "text/css;charset=UTF-8"
    rawLazy $(embedFile "static/style.css")

setContentType :: Text -> ActionC ()
setContentType = setHeader "Content-Type"

rawLazy :: ByteString -> ActionC ()
rawLazy =  raw . fromStrict

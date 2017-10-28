{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Text.Lazy (pack)
import System.Directory (listDirectory)
import Control.Monad
import Control.Monad.Trans
import Data.Monoid
import Notes

main :: IO ()
main = scotty 3000 $ do
    get "/" $ file "index.html"
    get "/script.js" $ file "script.js"
    get "/style.css" $ file "style.css"
    get "/notes" $ json [Note 1 "a" "b", Note 2 "How to commit in git" "like so: 1. open terminal 2. write git commit"]
    notFound $ html "<h1>Not found!</h1>"

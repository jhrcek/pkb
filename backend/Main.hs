{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Default.Class (def)
import qualified Loader
import Web.Scotty.Trans (Options, get, html, json, notFound, scottyOptsT, verbose)

import Config (Config)
import qualified Config
import qualified Pkb.Static as Static

main :: IO ()
main = do
    config <- Config.parseConfig
    putStrLn "Listening on http://localhost:3000"
    scottyOptsT quietOpts (runConfig config) $ do
        Static.staticAssets
        get "/notes" $ do
            notes <- liftIO $ Loader.loadNotes (Config.notesDir config)
            json notes
        notFound $ html "There is nothing here, pal!"

quietOpts :: Options
quietOpts = def {verbose = 0}

runConfig :: Config -> ReaderT Config IO a -> IO a
runConfig config m = runReaderT m config

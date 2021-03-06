{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (Config)
import qualified Config
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Default.Class (def)
import qualified Handler.Notes as Notes
import qualified Handler.Static as Static
import Web.Scotty.Trans (Options, html, notFound, scottyOptsT, verbose)

main :: IO ()
main = do
  config <- Config.parse
  putStrLn "Listening on http://localhost:3000"
  scottyOptsT quietOpts (runConfig config) $ do
    Static.assetHandler
    Notes.apiHandler
    notFound $ html "There is nothing here, pal!"

quietOpts :: Options
quietOpts = def {verbose = 0}

runConfig :: Config -> ReaderT Config IO a -> IO a
runConfig config m = runReaderT m config

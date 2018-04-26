{-# LANGUAGE TemplateHaskell #-}
module Data.Note where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Text.Lazy

data Note = Note
    { nId    :: !Int
    , nFile  :: !FilePath
    , nTitle :: !Text
    , nBody  :: !Text
    } deriving (Show)

$(deriveJSON defaultOptions ''Note)

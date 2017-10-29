{-# LANGUAGE OverloadedStrings #-}
module Notes where

import Data.Aeson
import Data.Text.Lazy

data Note = Note
    { nId    :: Int
    , nTitle :: Text
    , nBody  :: Text
    } deriving Show

instance ToJSON Note where
    toJSON (Note nId nTitle nBody) =
        object ["nId" .= nId, "nTitle" .= nTitle, "nBody" .= nBody]

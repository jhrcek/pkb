{-# LANGUAGE DeriveGeneric #-}
module Notes where

import Data.Aeson
import Data.Text.Lazy
import GHC.Generics

data Note = Note
    { nId    :: Int
    , nTitle :: Text
    , nBody  :: Text
    } deriving (Show, Generic)

instance ToJSON Note where

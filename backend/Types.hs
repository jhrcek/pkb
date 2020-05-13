module Types (ScottyC, ActionC) where

import Config
import Control.Monad.Reader (ReaderT)
import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ActionT, ScottyT)

type ScottyC = ScottyT Text ((ReaderT Config) IO)

type ActionC = ActionT Text ((ReaderT Config) IO)

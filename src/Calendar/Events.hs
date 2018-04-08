{-# LANGUAGE DeriveGeneric #-}
module Calendar.Events
  (
    Events(..),
    Event(..)
  ) where

import           Data.Aeson
import qualified Data.Text       as T
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           GHC.Generics

data Event =
  Event { title :: !T.Text
        , start :: !UTCTime
    } deriving (Show)

instance FromJSON Event where
  parseJSON (Object o) = do
    start <- o .: "start"

    Event <$> o .: "summary"
          <*> start .: "dateTime"

data Events =
  Events { items :: ![Event]
    } deriving (Show, Generic)
instance FromJSON Events

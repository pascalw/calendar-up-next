{-# LANGUAGE DeriveGeneric #-}
module Calendar.Events
  (
    Events(..),
    Event(..)
  ) where

import           Data.Aeson
import qualified Data.Text       as T
import           Data.Time.Clock (UTCTime)
import           GHC.Generics

data Event =
  Event { title    :: !T.Text
        , start    :: Maybe UTCTime
        , location :: Maybe T.Text
    } deriving (Show)

instance FromJSON Event where
  parseJSON (Object o) = do
    start <- o .: "start"

    Event <$> o .: "summary"
          <*> start .:? "dateTime"
          <*> o .:? "location"

data Events =
  Events { items :: ![Event]
    } deriving (Show, Generic)
instance FromJSON Events

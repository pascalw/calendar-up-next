module Calendar.Credentials
  (
    Credentials(..)
  ) where

import qualified Data.Text as T

data Credentials =
  Credentials { cServiceAccount :: T.Text, cServiceAccountKeyPath :: String } deriving (Show)

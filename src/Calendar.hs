module Calendar
    (
    getNextEvent,
    getNextEventFromNow,
    Event(..),
    CalendarAccount
    ) where

import           Calendar.Credentials
import           Calendar.Events
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens           (key, _String)
import           Data.List                 (stripPrefix)
import qualified Data.List                 as List
import           Data.String.Conversions   (cs)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import           Data.Time.Clock           (UTCTime, addUTCTime, diffUTCTime,
                                            getCurrentTime)
import           Data.Time.Format          (defaultTimeLocale, formatTime)
import           Network.Google.OAuth2.JWT as JWT
import           Network.Wreq
import           Text.Printf

type CalendarAccount = T.Text
type ServiceAccount = T.Text

createJWT :: Credentials -> IO String
createJWT (Credentials cServiceAccount cServiceAccountKeyPath) = do
  key <- fromPEMFile cServiceAccountKeyPath
  (Right signedJWT) <- JWT.getSignedJWT cServiceAccount (Just cServiceAccount) [scope] Nothing key
  return $ show signedJWT

  where
    scope = "https://www.googleapis.com/auth/calendar.readonly"

getAuthToken :: Credentials -> IO T.Text
getAuthToken credentials = do
  jwt <- createJWT credentials

  r <- post "https://www.googleapis.com/oauth2/v4/token" [
      "grant_type" := ("urn:ietf:params:oauth:grant-type:jwt-bearer" :: String),
      "assertion" := (jwt :: String)
    ]

  return $ r ^. responseBody . key "access_token" . _String

rfc3339 :: UTCTime -> T.Text
rfc3339 = cs . formatTime defaultTimeLocale "%0Y-%m-%dT%H:%M:%SZ"

calendarApiUrl :: CalendarAccount -> String
calendarApiUrl account = printf "https://www.googleapis.com/calendar/v3/calendars/%s/events" account

getNextEvent :: CalendarAccount -> Credentials -> UTCTime -> IO (Maybe Event)
getNextEvent calendarAccount credentials fromTime = do
  token <- getAuthToken credentials
  let (from, to) = next15minutesRange fromTime

  let opts = defaults & param "singleEvents" .~ ["true"]
                      & param "maxResults" .~ ["2"]
                      & param "timeMin" .~ [rfc3339 from]
                      & param "timeMax" .~ [rfc3339 to]
                      & header "Authorization" .~ [cs $ "Bearer " ++ (cs token)]

  r <- asJSON =<< getWith opts (calendarApiUrl calendarAccount)
  let (Events items) = ((r ^. responseBody) :: Events)
  return $ firstUnstarted fromTime items

notStarted :: UTCTime -> Event -> Bool
-- notStarted time event = diffUTCTime (start event) time > 0
notStarted time event =
  case start event of Nothing        -> False
                      Just startTime -> diffUTCTime startTime time > 0

firstUnstarted :: UTCTime -> [Event] -> (Maybe Event)
firstUnstarted _ [] = Nothing
firstUnstarted fromTime events =
  case unstarted of []        -> Nothing
                    (event:_) -> Just event
  where unstarted = filter (notStarted fromTime) events

getNextEventFromNow :: CalendarAccount -> Credentials -> IO (Maybe Event)
getNextEventFromNow calendarAccount credentials = do
  now <- getCurrentTime
  getNextEvent calendarAccount credentials now

next15minutesRange now = (from, to)
  where from = now
        to = addUTCTime (60 * 15) now

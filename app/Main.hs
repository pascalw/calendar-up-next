module Main where

import qualified Calendar
import           Calendar.Credentials
import           Data.Semigroup       ((<>))
import qualified Data.Text            as T
import           Data.Time.Format     (FormatTime, defaultTimeLocale,
                                       formatTime)
import           Data.Time.LocalTime  (TimeZone, getCurrentTimeZone,
                                       utcToLocalTime)
import           Options.Applicative
import           Text.Printf

data CalendarConfig = CalendarConfig
  { account               :: T.Text
  , serviceAccount        :: T.Text
  , serviceAccountKeyPath :: String }

parsedCmdArgs :: Parser CalendarConfig
parsedCmdArgs = CalendarConfig
  <$> strOption
      ( long "calendar-account"
     <> metavar "ACCOUNT"
     <> help "Account to retrieve upcoming events for" )
  <*> strOption
      ( long "service-account"
     <> metavar "ACCOUNT"
     <> help "Service account to use for authentication" )
  <*> strOption
      ( long "service-account-key-path"
     <> metavar "PATH"
     <> help "Path to service account credentials (.pem)" )

formatTime' :: FormatTime t => t -> String
formatTime' = formatTime defaultTimeLocale "%H:%M"

formatEvent :: Calendar.Event -> TimeZone -> String
formatEvent event timezone = printf "%s [%s]" title formattedStart
  where title = Calendar.title event
        formattedStart = formatTime' localStart
        start = Calendar.start event
        localStart = utcToLocalTime timezone start

main :: IO ()
main = getNextEvent =<< execParser opts
  where
    opts = info (parsedCmdArgs <**> helper)
      ( fullDesc
     <> progDesc "Get upcoming calendar event")

getNextEvent :: CalendarConfig -> IO ()
getNextEvent calendarConfig = do
  nextEvent <- Calendar.getNextEventFromNow (account calendarConfig) credentials
  tz <- getCurrentTimeZone

  case nextEvent of
    Nothing      -> return ()
    (Just event) -> putStrLn $ formatEvent event tz

  where credentials = Credentials {
    cServiceAccount = (serviceAccount calendarConfig),
    cServiceAccountKeyPath = (serviceAccountKeyPath calendarConfig)
  }

module Test.Main where

import Prelude
import Text.Parsing.Simple
import Text.Parsing.Combinators
import Control.Monad.Eff.Console (logShow)

type Year = Int
type Day = Int
data Month = Jan
           | Feb
           | Mar
           | Apr
           | May
           | Jun
           | Jul
           | Aug
           | Sep
           | Oct
           | Nov
           | Dec
data Date = Date Year Month Day

instance showMonth :: Show Month where
  show m = case m of
                Jan -> "Jan"
                Feb -> "Feb"
                Mar -> "Mar"
                Apr -> "Apr"
                May -> "May"
                Jun -> "Jun"
                Jul -> "Jul"
                Aug -> "Aug"
                Sep -> "Sep"
                Oct -> "Oct"
                Nov -> "Nov"
                Dec -> "Dec"

instance showDate :: Show Date where
  show (Date y m d) = "Year " <> show y <> ", Month " <> show m <> ", Day " <> show d

dateString :: String
dateString = "Jan 31, 2001"

dateParser :: Parser Date
dateParser = do
  month <- parseMonth
  space
  day <- int
  char ','
  space
  year <- int
  pure $ Date year month day
    where
    parseMonth :: Parser Month
    parseMonth = do
      m <- parseMonthStr
      pure case m of
                "Jan" -> Jan
                "Feb" -> Feb
                "Mar" -> Mar
                "Apr" -> Apr
                "May" -> May
                "Jun" -> Jun
                "Jul" -> Jul
                "Aug" -> Aug
                "Sep" -> Sep
                "Oct" -> Oct
                "Nov" -> Nov
                "Dec" -> Dec
                _ -> Jan

    parseMonthStr :: Parser String
    parseMonthStr =
      choice [ string "Jan"
             , string "Feb"
             , string "Mar"
             , string "Apr"
             , string "May"
             , string "Jun"
             , string "Jul"
             , string "Aug"
             , string "Sep"
             , string "Oct"
             , string "Nov"
             , string "Dec"
             ]

main = logShow $ parse dateParser dateString

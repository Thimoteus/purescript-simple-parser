module Test.Main where

import Prelude
import Text.Parsing.Simple
import Text.Parsing.Combinators (sepBy, choice)
import Control.Monad.Eff.Console (logShow)
import Data.List (List)

main = do
  logShow $ parse dateParser dateString
  logShow $ parse expr testSexpr

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

-- | Simple s-expression lang

data Expr = Lit Int
          | List (List Expr)

instance showExpr :: Show Expr where
  show (Lit n) = show n
  show (List xs) = show xs

parseLit :: Parser Expr
parseLit = Lit <$> int

parseList :: Parser Expr -> Parser Expr
parseList p = List <$> do
  char '('
  es <- p `sepBy` whitespace
  char ')'
  pure es

testSexpr :: String
testSexpr = "(1 (2 3))"

expr :: Parser Expr
expr = fix f where
  f p = parseList p <| parseLit

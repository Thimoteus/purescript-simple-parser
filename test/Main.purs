module Test.Main where

import Text.Parsing.Simple
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.List (List)
import Prelude (class Show, Unit, bind, unit, pure, show, (<$>), (==), (||), ($), (/=), (<>))
import Text.Parsing.Combinators (choice, bracket)

main :: forall a. Eff ( console :: CONSOLE | a) Unit
main = do
  logShow $ parse dateParser dateString
  logShow $ parse exprs testSexpr

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

dateParser :: Parser String Date
dateParser = do
  month <- parseMonth
  space
  day <- int
  char ','
  space
  year <- int
  pure $ Date year month day
    where
    parseMonth :: Parser String Month
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
                _ -> Dec

    parseMonthStr :: Parser String String
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
          | Atom String
          | List (List Expr)

instance showExpr :: Show Expr where
  show (Lit n) = show n
  show (List xs) = show xs
  show (Atom s) = s

skipmany :: forall a. Parser String a -> Parser String Unit
skipmany p = skipsome p <| pure unit

skipsome :: forall a. Parser String a -> Parser String Unit
skipsome p = do
  x <- p
  xs <- skipmany p
  pure unit

singleComment :: Parser String Unit
singleComment = do
  char ';'
  skipmany $ sat (_ /= '\n')

simplespace :: Parser String Unit
simplespace = skipsome $ sat \ c -> c == ' ' || c == '\n' || c == '\r' || c == '\t'

spaces :: Parser String Unit
spaces = skipmany (simplespace <| singleComment)

parseLit :: Parser String Expr
parseLit = Lit <$> int

parseAtom :: Parser String Expr
parseAtom = Atom <$> someChar alphanum

parseList :: Parser String Expr -> Parser String Expr
parseList p = List <$> p `sepBy` whitespace

testSexpr :: String
testSexpr = "(add (mul (div 2 (add 9 9)) (sub 9 99)) 18)"

expr :: Parser String Expr
expr = fix f where
  f p = parseLit <| parseAtom <| bracket (char '(') (parseList p) (char ')')

exprs :: Parser String (List Expr)
exprs = do
  spaces
  expr `sepBy` whitespace

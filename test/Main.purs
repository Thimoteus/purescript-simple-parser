module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Effect (Effect)
import Effect.Console (log)
import Control.Monad.Transformerless.Except (Except(..))
import Data.List (List)
import Text.Parsing.Simple (Parser, alphaNum, bracket, char, choice, int, parse, satisfy, sepBy, skipMany, skipSome, someChar, space, string, whiteSpace)

main :: Effect Unit
main = do
  log $ showExcept $ parse dateParser dateString
  log $ showExcept $ parse exprs testSexpr

showExcept :: forall a b. Show a => Show b => Except a b -> String
showExcept (Except e) = "(Except " <> show e <> ")"

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
  _ <- space
  day <- int
  _ <- char ','
  _ <- space
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

singleComment :: Parser String Unit
singleComment = do
  _ <- char ';'
  skipMany $ satisfy (_ /= '\n')

simplespace :: Parser String Unit
simplespace = skipSome $ satisfy \ c -> c == ' ' || c == '\n' || c == '\r' || c == '\t'

spaces :: Parser String Unit
spaces = skipMany (simplespace <|> singleComment)

parseLit :: Parser String Expr
parseLit = Lit <$> int

parseAtom :: Parser String Expr
parseAtom = Atom <$> someChar alphaNum

parseList :: Parser String Expr -> Parser String Expr
parseList p = List <$> p `sepBy` whiteSpace

testSexpr :: String
testSexpr = "(add (mul (div 2 (add 9 9)) (sub 9 99)) 18)"

expr :: Parser String Expr
expr = fix f where
  f p = parseLit <|> parseAtom <|> bracket (char '(') (char ')') (parseList p)

exprs :: Parser String (List Expr)
exprs = do
  spaces
  expr `sepBy` whiteSpace

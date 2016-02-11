module Text.Parsing.Simple
  -- definitional exports
  ( Parser()
  , runParser
  , parse
  -- utility functions
  , fromCharList
  -- polymorphic Parser-specific combinators
  , none
  , try
  , fix
  , lookahead
  , isn't
  , notFollowedBy
  , skip
  -- non-polymorphic Parsers
  , item
  , sat
  , isn'tAny
  , char
  , string
  , digit
  , lower
  , upper
  , letter
  , alphanum
  , space
  , tab
  , newline
  , cr
  , whitespace
  , whitespaces
  , skipSpaces
  , eof
  , int
  , number
  , boolean
  ) where

import Prelude (class Applicative, class Monad, class Bind, class Apply, class Functor, class Semigroup, Unit, pure, bind, (++), ($), (<$>), (<=), (&&), (>=), (==), unit, (<*>), (<<<))
import Global (readFloat)

import Control.Alt (class Alt, (<|>), alt)
import Control.Plus (class Plus, empty)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.Apply ((*>))

import Data.Monoid (class Monoid)
import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable, foldMap, notElem)
import Data.String (uncons, fromChar)
import Data.List (List(), (:))
import Data.Int (fromString)

import Text.Parsing.Combinators (many)

newtype Parser a = Parser (String -> { consumed :: Maybe a, remaining :: String })

-- | Unwraps the `newtype`, giving you a function which takes a `String` and
-- | returns a product of already-parsed data and the remaining `String`.
runParser :: forall a. Parser a -> String -> { consumed :: Maybe a, remaining :: String }
runParser (Parser x) = x

-- | Run a given parser against a `String`, maybe getting a value or nothing.
parse :: forall a. Parser a -> String -> Maybe a
parse p = _.consumed <<< runParser p

instance semigroupParser :: Semigroup (Parser a) where
  append = alt

instance monoidParser :: Monoid (Parser a) where
  mempty = none

instance functorParser :: Functor Parser where
  map f (Parser p) = Parser \ str ->
    { consumed: f <$> (p str).consumed, remaining: (p str).remaining }

instance altParser :: Alt Parser where
  alt (Parser x) (Parser y) =
    Parser \ str -> case (x str).consumed of
                         Just _ -> x str
                         _ -> y str

instance plusParser :: Plus Parser where
  empty = none

instance applyParser :: Apply Parser where
  apply (Parser f) (Parser x) =
    Parser \ str ->
      let f' = f str
          x' = x f'.remaining
          consumed = f'.consumed <*> x'.consumed
          remaining = x'.remaining
       in { consumed, remaining }

instance applicativeParser :: Applicative Parser where
  pure x = Parser \ str -> { consumed: Just x, remaining: str }

instance bindParser :: Bind Parser where
  bind (Parser mx) mf = Parser \ str ->
    let x = mx str
     in case x.consumed of
             Just y -> runParser (mf y) x.remaining
             _ -> { consumed: Nothing, remaining: str }

instance monadParser :: Monad Parser

instance alternativeParser :: Alternative Parser

instance monadPlusParser :: MonadPlus Parser

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = foldMap fromChar

-- | Always fail.
none :: forall a. Parser a
none = Parser \ str -> { consumed: Nothing, remaining: str }

-- | If the given parser fails, return to the point of failure.
try :: forall a. Parser a -> Parser a
try (Parser x) = Parser \ str ->
  case (x str).consumed of
       Just _ -> x str
       _ -> { consumed: Nothing, remaining: str }

-- | Find a function's least fixed point.
fix :: forall a. (Parser a -> Parser a) -> Parser a
fix f = Parser \ str -> runParser (f (fix f)) str

-- | Parse without consuming input.
lookahead :: forall a. Parser a -> Parser a
lookahead (Parser x) = Parser \ str ->
  let parsed = x str
   in parsed { remaining = str }

-- | `isn't p` succeeds iff p fails, though it will always consume the same
-- | amount of string that p does.
isn't :: forall a. Parser a -> Parser Unit
isn't (Parser x) = Parser \ str ->
  let parsed = x str
   in case parsed.consumed of
           Just _ -> parsed { consumed = Nothing }
           _ -> parsed { consumed = Just unit }

-- | Differs from `isn't` in that this never consumes input.
notFollowedBy :: forall a. Parser a -> Parser Unit
notFollowedBy (Parser x) = Parser \ str ->
  let parsed = x str
   in case parsed.consumed of
           Just _ -> { consumed: Nothing, remaining: str }
           _ -> { consumed: Just unit, remaining: str }

-- | Discard the result of a parse.
skip :: forall a. Parser a -> Parser Unit
skip (Parser p) =
  Parser \ str -> { consumed: Just unit, remaining: (p str).remaining }

-- | Parse a single `Char`.
item :: Parser Char
item = Parser \ str ->
  case uncons str of
       Just { head, tail } -> { consumed: Just head, remaining: tail }
       _ -> { consumed: Nothing, remaining: str }

-- | Create a parser from a characteristic function.
sat :: (Char -> Boolean) -> Parser Char
sat p = do
  x <- item
  if p x then pure x else empty

-- | Match any character not in the foldable container.
isn'tAny :: forall f. Foldable f => f Char -> Parser Char
isn'tAny xs = sat (`notElem` xs)

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string "" = pure ""
string x =
  case uncons x of
       Just y -> char y.head *> string y.tail *> pure (fromChar y.head ++ y.tail)
       _ -> pure ""

digit :: Parser Char
digit = sat \ x -> x >= '0' && x <= '9'

-- | Parse a lowercase character.
lower :: Parser Char
lower = sat \ x -> x >= 'a' && x <= 'z'

-- | Parse an uppercase character.
upper :: Parser Char
upper = sat \ x -> x >= 'A' && x <= 'Z'

letter :: Parser Char
letter = lower <|> upper

-- | Parse a letter or a digit.
alphanum :: Parser Char
alphanum = letter <|> digit

space :: Parser Char
space = char ' '

tab :: Parser Char
tab = char '\t'

newline :: Parser Char
newline = char '\n'

-- | Parse a carriage return.
cr :: Parser Char
cr = char '\r'

whitespace :: Parser Char
whitespace = space <|> tab <|> newline <|> cr

whitespaces :: Parser (List Char)
whitespaces = many whitespace

skipSpaces :: Parser Unit
skipSpaces = skip whitespaces

-- | Parse the end of a file, returning `Unit` to indicate success.
eof :: Parser Unit
eof = notFollowedBy item

numerals :: Parser String
numerals = do
  first <- digit <|> char '-'
  digits <- many digit
  pure $ fromCharList (first : digits)

-- | Parse an `Int`. Note that this parser will fail if the candidate would be
-- | outside the range of allowable `Int`s.
int :: Parser Int
int = do
  n <- numerals
  case fromString n of
       Just x -> pure x
       _ -> empty

-- | Parse a `Number`. The string must have digits surrounding a decimal point.
number :: Parser Number
number = do
  integral <- numerals
  char '.'
  fractional <- fromCharList <$> many digit
  pure $ readFloat $ integral ++ "." ++ fractional

boolean :: Parser Boolean
boolean = do
  x <- string "true" <|> string "false"
  pure case x of
            "true" -> true
            _ -> false

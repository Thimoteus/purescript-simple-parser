module Text.Parsing.Simple
  -- definitional exports
  ( Parser()
  , runParser
  , parse
  -- generalized combinators
  , choice
  , option
  , bracket
  , many
  , many1
  , fromCharList
  -- Parser-specific combinators
  , none
  , item
  , sepBy
  , sat
  , try
  , fix
  , lookahead
  , notFollowedBy
  , skip
  , string
  -- Specific Parsers
  , digit
  , char
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
import Control.Apply ((*>), (<*))

import Data.Monoid (class Monoid)
import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable, foldMap, foldl)
import Data.String (uncons, fromChar)
import Data.List (List(..), (:))
import Data.Int (fromString)

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
                         Nothing -> y str

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
             Nothing -> { consumed: Nothing, remaining: str }

instance monadParser :: Monad Parser

instance alternativeParser :: Alternative Parser

instance monadPlusParser :: MonadPlus Parser

-- | Choose the first successful element from a foldable container of parsers.
choice :: forall f m a. (Plus m, Foldable f) => f (m a) -> m a
choice = foldl alt empty

-- | Attempt a parse, with a default value in case of failure.
option :: forall m a. Alternative m => a -> m a -> m a
option x p = p <|> pure x

-- | Parse something surrounded by given arguments.
bracket :: forall m l r a. Applicative m => m l -> m a -> m r -> m a
bracket left target right = left *> target <* right

-- | Parse as many times as possible, giving a `List`.
many :: forall m a. MonadPlus m => m a -> m (List a)
many p = parsed <|> pure Nil where
  parsed = do
    x <- p
    xs <- many p
    pure (x : xs)

-- | Parse at least once, giving a `List`.
many1 :: forall m a. MonadPlus m => m a -> m (List a)
many1 p = Cons <$> p <*> many p

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = foldMap fromChar

-- | Always fail.
none :: forall a. Parser a
none = Parser \ str -> { consumed: Nothing, remaining: str }

-- | Parse a single `Char`.
item :: Parser Char
item = Parser \ str ->
  case uncons str of
       Just { head, tail } -> { consumed: Just head, remaining: tail }
       Nothing -> { consumed: Nothing, remaining: str }

-- | Given a value to parse and a separating parser, put all the values it finds
-- | into a `List`.
sepBy :: forall a b. Parser a -> Parser b -> Parser (List a)
sepBy target separator = many $ target <* skip separator

-- | Create a parser from a characteristic function.
sat :: (Char -> Boolean) -> Parser Char
sat p = do
  x <- item
  if p x then pure x else empty

-- | If the given parser fails, return to the point of failure.
try :: forall a. Parser a -> Parser a
try (Parser x) = Parser \ str ->
  case (x str).consumed of
       Just _ -> x str
       Nothing -> { consumed: Nothing, remaining: str }

-- | Find a function's least fixed point.
fix :: forall a. (Parser a -> Parser a) -> Parser a
fix f = Parser \ str -> runParser (f (fix f)) str

-- | Parse without consuming input.
lookahead :: forall a. Parser a -> Parser a
lookahead (Parser x) = Parser \ str ->
  let parsed = x str
   in parsed { remaining = str }

notFollowedBy :: forall a. Parser a -> Parser Unit
notFollowedBy (Parser x) = Parser \ str ->
  let parsed = x str
   in case parsed.consumed of
           Just _ -> parsed { consumed = Nothing }
           _ -> parsed { consumed = Just unit }

-- | Discard the result of a parse.
skip :: forall a. Parser a -> Parser Unit
skip (Parser p) =
  Parser \ str -> { consumed: Just unit, remaining: (p str).remaining }

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
       Nothing -> empty

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

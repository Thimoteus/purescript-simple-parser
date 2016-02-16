module Text.Parsing.Simple
  -- definitional exports
  ( Parser(), runParser, parse
  -- utility functions
  , altL, (<|<)
  , altR, (>|>)
  , fromCharList
  -- polymorphic Parser-specific combinators
  , none
  , try
  , many
  , some
  , fix
  , lookahead
  , isn't
  , notFollowedBy
  , skip
  , suchThat, (|=)
  -- non-polymorphic Parsers
  , item
  , sat
  , isn'tAnyF, isn'tAny
  , anyOfF, anyOf
  , char, string
  , digit, lower, upper, letter, alphanum
  , space, tab, newline, cr
  , whitespace, whitespaces, skipSpaces
  , word
  , eof
  , int, number, boolean
  ) where

import Prelude (class Applicative, class Monad, class Bind, class Apply, class Functor, class Semigroup, Unit, pure, bind, (<>), ($), (<$>), (<=), (&&), (>=), (==), unit, (<*>), (/=))
import Global (readFloat)

import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus, empty)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.Lazy (class Lazy)

import Data.Monoid (class Monoid)
import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable, foldMap, notElem, elem)
import Data.String (fromChar, indexOf, drop, length, charAt, contains)
import Data.List (List(..), (:), reverse)
import Data.Int (fromString)

type ParserT s a = s -> { consumed :: Maybe a, remaining :: s }

newtype Parser a = Parser (ParserT String a)

-- | Unwraps the `newtype`, giving you a function which takes a `String` and
-- | returns a product of already-parsed data and the remaining `String`.
runParser :: forall a. Parser a -> String -> { consumed :: Maybe a, remaining :: String }
runParser (Parser x) = x

-- | Run a given parser against a `String`, maybe getting a value or nothing.
parse :: forall a. Parser a -> String -> Maybe a
parse (Parser p) input = (p input).consumed

instance semigroupParser :: Semigroup (Parser a) where
  append = altL

instance monoidParser :: Monoid (Parser a) where
  mempty = none

instance lazyParser :: Lazy (Parser a) where
  defer f = Parser \ str -> runParser (f unit) str

instance functorParser :: Functor Parser where
  map f (Parser p) = Parser \ str ->
    let x = p str
     in { consumed: f <$> x.consumed, remaining: x.remaining }

instance altParser :: Alt Parser where
  alt = altL

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

altL :: forall a. Parser a -> Parser a -> Parser a
altL (Parser x) (Parser y) =
  Parser \ str -> let z = x str
                   in case z.consumed of
                       Just _ -> z
                       _ -> y str

infixl 3 altL as <|<

altR :: forall a. Parser a -> Parser a -> Parser a
altR (Parser x) (Parser y) =
  Parser \ str -> let z = y str
                   in case z.consumed of
                       Just _ -> z
                       _ -> x str

infixr 3 altR as >|>

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = foldMap fromChar

-- | Always fail.
none :: forall a. Parser a
none = Parser \ str -> { consumed: Nothing, remaining: str }

-- | If the given parser fails, return to the point of failure.
try :: forall a. Parser a -> Parser a
try (Parser x) = Parser \ str ->
  let y = x str
   in case y.consumed of
       Just _ -> y
       _ -> { consumed: Nothing, remaining: str }

-- | Attempt a parse as many times as possible, putting all successes into
-- | a list.
-- | WARNING: Applying this to a parser which never fails and never consumes
-- | input will result in a bottom, i.e. a nonterminating program.
many :: forall a. Parser a -> Parser (List a)
many p = Parser \ str -> go str p Nil
  where
    go curr f acc =
      let y = runParser f curr
       in case y.consumed of
               Just z -> go y.remaining f (z : acc)
               _ -> { consumed: Just (reverse acc), remaining: curr }

-- | Attempt a parse one or more times.
some :: forall a. Parser a -> Parser (List a)
some p = Cons <$> p <*> many p

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

-- | Attempt a parse subject to a predicate. If the parse succeeds but the
-- | predicate fails, the parse fails without backtracking.
-- | If the parse fails, it will backtrack.
suchThat :: forall a. Parser a -> (a -> Boolean) -> Parser a
suchThat (Parser p) f = Parser \ str ->
  let parsed = p str
   in case parsed.consumed of
           Just res -> if f res then parsed else parsed { consumed = Nothing }
           _ -> { consumed: Nothing, remaining: str }

infixl 5 suchThat as |=

-- | Parse a single `Char`.
item :: Parser Char
item = Parser \ str -> { consumed: charAt 0 str, remaining: drop 1 str }

-- | Create a parser from a `Char`acteristic function.
sat :: (Char -> Boolean) -> Parser Char
sat f = Parser \ str ->
        let mc = charAt 0 str
         in case f <$> mc of
                 Just true -> { consumed: mc, remaining: drop 1 str }
                 _ -> { consumed: Nothing, remaining: str }

-- | Match any character not in the foldable container.
isn'tAnyF :: forall f. Foldable f => f Char -> Parser Char
isn'tAnyF xs = sat (`notElem` xs)

-- | Match any character not in the string.
-- | Equivalent to `isn'tAnyF <<< toCharArray`.
isn'tAny :: String -> Parser Char
isn'tAny s =
  Parser \ str ->
    case charAt 0 str of
         Just c -> if contains (fromChar c) s
                      then { consumed: Nothing, remaining: str }
                      else { consumed: Just c, remaining: drop 1 str }
         _ -> { consumed: Nothing, remaining: str }

-- | Match any character in the foldable container.
anyOfF :: forall f. Foldable f => f Char -> Parser Char
anyOfF xs = sat (`elem` xs)

-- | Match any character in the string.
-- | Equivalent to `anyOfF <<< toCharArray`.
anyOf :: String -> Parser Char
anyOf s =
  Parser \ str ->
    case charAt 0 str of
         Just c -> if contains (fromChar c) s
                      then { consumed: Just c, remaining: drop 1 str }
                      else { consumed: Nothing, remaining: str }
         _ -> { consumed: Nothing, remaining: str }

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string s = Parser \ str ->
           case indexOf s str of
                Just 0 -> { consumed: Just s, remaining: drop (length s) str }
                _ -> { consumed: Nothing, remaining: str }

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

-- | Contiguous strings with no tabs, spaces, carriage returns or newlines.
word :: Parser String
word = fromCharList <$> many (sat \ c ->
  c /= ' ' && c /= '\t' && c /= '\r' && c /= '\n')

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
  pure $ readFloat $ integral <> "." <> fractional

boolean :: Parser Boolean
boolean = do
  x <- string "true" <|> string "false"
  pure case x of
            "true" -> true
            _ -> false

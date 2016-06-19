module Text.Parsing.Simple
  -- definitional exports
  ( Parser, ParseError, parse
  -- utility functions
  , altL, (<|)
  , altR, (|>)
  , applyL, (<<)
  , applyR, (>>)
  , fromCharList
  -- polymorphic Parser-specific combinators
  , none
  , fail, orFailWith, (<?>)
  , try
  , many , some
  , fix
  , lookahead
  , isn't
  , notFollowedBy
  , skip
  , suchThat, (|=)
  , sepBy, sepBy1
  -- non-polymorphic Parsers
  , item
  , sat, sat'
  , isn'tAnyF, isn'tAnyF', isn'tAny, isn'tAny'
  , anyOfF, anyOfF', anyOf, anyOf'
  , char, char', string, string'
  , tail
  , digit, digit', lower, lower', upper, upper', letter, letter', alphanum, alphanum'
  , space, space', tab, tab', newline, newline', cr, cr'
  , whitespace, whitespace', whitespaces, skipSpaces
  , someChar, manyChar
  , word
  , eof
  , integral, int, number, boolean
  ) where

import Prelude
import Global (readFloat)

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Control.Alternative (class Alternative)
import Control.MonadZero (class MonadZero)
import Control.MonadPlus (class MonadPlus)
import Control.Lazy (class Lazy)

import Data.Monoid (class Monoid)
import Data.Maybe (Maybe(Just))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, notElem, elem)
import Data.String (singleton, indexOf, drop, length, charAt, contains, take)
import Data.List (List(..), (:), reverse)
import Data.Int (fromString)
import Data.Functor ((<$))

type ParseError = String

data Result a = Result (Either ParseError a) String

consumed :: forall a. Result a -> Either ParseError a
consumed (Result c _) = c

remaining :: forall a. Result a -> String
remaining (Result _ r) = r

newtype Parser a = Parser (String -> Result a)

-- | Unwraps the `newtype`, giving you a function which takes a `String` and
-- | returns a product of already-parsed data and the remaining `String`.
runParser :: forall a. Parser a -> String -> Result a
runParser (Parser x) = x

-- | Run a parser against a `String`, either getting an error or a value.
parse :: forall a. Parser a -> String -> Either ParseError a
parse (Parser p) = consumed <<< p

instance semigroupParser :: Semigroup (Parser a) where
  append (Parser x) (Parser y) = Parser \ str ->
    let z = x str
     in case consumed z of
             Right _ -> z
             _ -> y (remaining z)

instance monoidParser :: Monoid (Parser a) where
  mempty = none

instance lazyParser :: Lazy (Parser a) where
  defer f = Parser \ str -> runParser (f unit) str

instance functorParser :: Functor Parser where
  map f (Parser p) = Parser \ str ->
    let x = p str
     in Result (f <$> consumed x) (remaining x)

instance altParser :: Alt Parser where
  alt = altL

instance plusParser :: Plus Parser where
  empty = none

instance applyParser :: Apply Parser where
  apply (Parser f) (Parser x) =
    Parser \ str ->
      let f' = f str
          x' = x (remaining f')
          con = consumed f' <*> consumed x'
          rem = remaining x'
       in Result con rem

instance applicativeParser :: Applicative Parser where
  pure x = Parser (Result (Right x))

instance bindParser :: Bind Parser where
  bind (Parser mx) mf = Parser \ str ->
    let x = mx str
     in case consumed x of
             Right y -> runParser (mf y) (remaining x)
             _ ->
               let msg = "Parse failed at " <> take 5 str <> "..."
                in Result (Left msg) str

instance monadParser :: Monad Parser

instance alternativeParser :: Alternative Parser

instance monadZeroParser :: MonadZero Parser

instance monadPlusParser :: MonadPlus Parser

altL :: forall a. Parser a -> Parser a -> Parser a
altL (Parser x) (Parser y) =
  Parser \ str -> let z = x str
                   in case consumed z of
                       Right _ -> z
                       _ -> y str

infixl 3 altL as <|

altR :: forall a. Parser a -> Parser a -> Parser a
altR (Parser x) (Parser y) =
  Parser \ str -> let z = y str
                   in case consumed z of
                       Right _ -> z
                       _ -> x str

infixr 3 altR as |>

-- | Equivalent to (<*) but faster since it doesn't require passing typeclass
-- | dictionaries.
applyL :: forall a b. Parser a -> Parser b -> Parser a
applyL (Parser f) (Parser g) = Parser \ str ->
  let x = f str
   in case consumed x of
           t@(Right _) ->
             let y = g (remaining x)
              in case consumed y of
                      Right _ -> Result t (remaining y)
                      Left s -> Result (Left s) (remaining x)
           _ -> x

infixl 4 applyL as <<

-- | Equivalent to (*>) but faster since it doesn't require passing typeclass
-- | dictionaries.
applyR :: forall a b. Parser a -> Parser b -> Parser b
applyR (Parser f) (Parser g) = Parser \ str ->
  let x = f str
   in case consumed x of
           (Right _) -> g (remaining x)
           (Left s) -> Result (Left s) (remaining x)

infixl 4 applyR as >>

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = foldMap singleton

-- | Always fail.
none :: forall a. Parser a
none = Parser \ str -> Result (Left "Parse failed on `none`") str

-- | Fail with a message
fail :: forall a. ParseError -> Parser a
fail msg = Parser \ str -> Result (Left msg) str

orFailWith :: forall a. Parser a -> ParseError -> Parser a
orFailWith (Parser p) msg = Parser \ str ->
  let parsed = p str
   in case consumed parsed of
           Right _ -> parsed
           _ -> Result (Left msg) str

infix 0 orFailWith as <?>

-- | If the given parser fails, backtrack to the point of failure.
try :: forall a. Parser a -> Parser a
try (Parser x) = Parser \ str ->
  let y = x str
   in case consumed y of
       Right _ -> y
       _ -> Result (Left "Parse failed on `try`") str

-- | Attempt a parse as many times as possible, putting all successes into
-- | a list.
-- | WARNING: Applying this to a parser which never fails and never consumes
-- | input will result in a bottom, i.e. a nonterminating program.
many :: forall a. Parser a -> Parser (List a)
many p = Parser \ str -> go str p Nil
  where
    go curr f acc =
      let y = runParser f curr
       in case consumed y of
               Right z -> go (remaining y) f (z : acc)
               _ -> Result (Right (reverse acc)) curr

-- | Attempt a parse one or more times.
some :: forall a. Parser a -> Parser (List a)
some p = Cons <$> p <*> many p

-- | Find a function's least fixed point using the Z combinator.
fix :: forall a. (Parser a -> Parser a) -> Parser a
fix f = Parser \ str -> runParser (f (fix f)) str

-- | Parse without consuming input.
lookahead :: forall a. Parser a -> Parser a
lookahead (Parser x) = Parser \ str -> (Result $ consumed $ x str) str

-- | `isn't p` succeeds iff p fails, though it will always consume the same
-- | amount of string that p does.
isn't :: forall a. Parser a -> Parser Unit
isn't (Parser x) = Parser \ str ->
  let parsed = x str
   in case consumed parsed of
           Right _ ->
             let msg = "Parse failed on `isn't` when trying to parse the string "
                    <> show (take 5 str)
                    <> "..."
              in Result (Left msg) (remaining parsed)
           _ -> Result (Right unit) (remaining parsed)

-- | Differs from `isn't` in that this never consumes input.
notFollowedBy :: forall a. Parser a -> Parser Unit
notFollowedBy (Parser x) =
  Parser \ str -> case consumed (x str) of
    Right _ ->
      let msg = "Parse failed on `notFollowedBy` when trying to parse the string "
             <> show (take 5 str)
             <> "..."
       in Result (Left msg) str
    _ -> Result (Right unit) str

-- | Discard the result of a parse.
skip :: forall a. Parser a -> Parser Unit
skip (Parser p) = Parser \ str -> Result (Right unit) (remaining $ p str)

-- | Attempt a parse subject to a predicate. If the parse succeeds but the
-- | predicate does not hold, the resulting parse fails *without* backtracking.
-- | If the parse fails, it will backtrack.
suchThat :: forall a. Parser a -> (a -> Boolean) -> Parser a
suchThat (Parser p) f = Parser \ str ->
  let parsed = p str
   in case consumed parsed of
           Right res ->
             if f res
                then parsed
                else let msg = "Predicate failed on `suchThat` when trying to parse the string "
                            <> show (take 5 str)
                            <> "..."
                      in Result (Left msg) (remaining parsed)
           _ -> Result (Left "Parse failed on `suchThat`") str

infixl 5 suchThat as |=

sepBy :: forall a b. Parser a -> Parser b -> Parser (List a)
sepBy target separator = sepBy1 target separator <| pure Nil

sepBy1 :: forall a b. Parser a -> Parser b -> Parser (List a)
sepBy1 target separator = Cons <$> target <*> many (separator >> target)

-- | Matches the unparsed portion of the input.
tail :: Parser String
tail = Parser \ str -> Result (Right str) ""

-- | Parse a single `Char`.
item :: Parser Char
item = Parser \ str ->
  case charAt 0 str of
       Just c -> Result (Right c) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

-- | ## Backtracking combinators

-- | Create a parser from a `Char`acteristic function.
sat :: (Char -> Boolean) -> Parser Char
sat f = Parser \ str ->
  case charAt 0 str of
       Just c ->
         if f c
            then Result (Right c) (drop 1 str)
            else let msg = "Character "
                        <> show c
                        <> " did not satisfy predicate when trying to parse the string "
                        <> show (take 5 str)
                        <> "..."
                  in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

-- | Match any character not in the foldable container.
isn'tAnyF :: forall f. Foldable f => f Char -> Parser Char
isn'tAnyF xs = sat (_ `notElem` xs)

-- | Match any character not in the string.
-- | Equivalent to `isn'tAnyF <<< toCharArray`.
isn'tAny :: String -> Parser Char
isn'tAny s = Parser \ str ->
  case charAt 0 str of
       Just c -> if contains (singleton c) s
                    then let msg = "Expecting none of "
                                <> show s
                                <> " but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
                    else Result (Right c) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

-- | Match any character in the foldable container.
anyOfF :: forall f. Foldable f => f Char -> Parser Char
anyOfF xs = sat (_ `elem` xs)

-- | Match any character in the string.
-- | Equivalent to `anyOfF <<< toCharArray`.
anyOf :: String -> Parser Char
anyOf s = Parser \ str ->
  case charAt 0 str of
       Just c -> if contains (singleton c) s
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected one of "
                                <> show s
                                <> " but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

char :: Char -> Parser Char
char x = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == x
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected "
                                <> show x
                                <> " but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

string :: String -> Parser String
string s = Parser \ str ->
  case indexOf s str of
       Just 0 -> Result (Right s) (drop (length s) str)
       _ -> let msg = "Expecting "
                   <> show s
                   <> " but found "
                   <> show (take (length s) str)
             in Result (Left msg) str

digit :: Parser Char
digit = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= '0' && c <= '9'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a digit but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

-- | Parse a lowercase character.
lower :: Parser Char
lower = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'a' && c <= 'z'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a lowercase character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

-- | Parse an uppercase character.
upper :: Parser Char
upper = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected an uppercase character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

letter :: Parser Char
letter = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a letter but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

-- | Parse a letter or a digit.
alphanum :: Parser Char
alphanum = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected alphanumeric character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

space :: Parser Char
space = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == ' '
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a space but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

tab :: Parser Char
tab = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\t'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a tab but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

newline :: Parser Char
newline = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\n'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a newline but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

-- | Parse a carriage return.
cr :: Parser Char
cr = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\r'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected carriage return but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

whitespace :: Parser Char
whitespace = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\r' || c == '\n' || c == '\t' || c == ' '
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected whitespace but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) str
       _ -> Result (Left "Reached end of file") str

-- | ## Non-backtracking combinators

sat' :: (Char -> Boolean) -> Parser Char
sat' f = Parser \ str ->
  case charAt 0 str of
       Just c ->
         if f c
            then Result (Right c) (drop 1 str)
            else let msg = "Character "
                        <> show c
                        <> " did not satisfy predicate when trying to parse the string "
                        <> show (take 5 str)
                        <> "..."
                  in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

isn'tAnyF' :: forall f. Foldable f => f Char -> Parser Char
isn'tAnyF' xs = sat' (_ `notElem` xs)

isn'tAny' :: String -> Parser Char
isn'tAny' s = Parser \ str ->
  case charAt 0 str of
       Just c -> if contains (singleton c) s
                    then let msg = "Expecting none of "
                                <> show s
                                <> " but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
                    else Result (Right c) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

anyOfF' :: forall f. Foldable f => f Char -> Parser Char
anyOfF' xs = sat' (_ `elem` xs)

anyOf' :: String -> Parser Char
anyOf' s = Parser \ str ->
  case charAt 0 str of
       Just c -> if contains (singleton c) s
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected one of "
                                <> show s
                                <> " but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

char' :: Char -> Parser Char
char' x = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == x
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected "
                                <> show x
                                <> " but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

string' :: String -> Parser String
string' s = Parser \ str ->
  case indexOf s str of
       Just 0 -> Result (Right s) (drop (length s) str)
       _ -> let msg = "Expecting "
                   <> show s
                   <> " but found "
                   <> show (take (length s) str)
             in Result (Left msg) (drop (length s) str)

digit' :: Parser Char
digit' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= '0' && c <= '9'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a digit but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

lower' :: Parser Char
lower' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'a' && c <= 'z'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a lowercase character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

upper' :: Parser Char
upper' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected an uppercase character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

letter' :: Parser Char
letter' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a letter but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

alphanum' :: Parser Char
alphanum' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected alphanumeric character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

space' :: Parser Char
space' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == ' '
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a space but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

tab' :: Parser Char
tab' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\t'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a tab but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

newline' :: Parser Char
newline' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\n'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected a newline but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

cr' :: Parser Char
cr' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\r'
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected carriage return but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

whitespace' :: Parser Char
whitespace' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\r' || c == '\n' || c == '\t' || c == ' '
                    then Result (Right c) (drop 1 str)
                    else let msg = "Expected whitespace but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in Result (Left msg) (drop 1 str)
       _ -> Result (Left "Reached end of file") str

-- | ## Higher-order combinators

whitespaces :: Parser (List Char)
whitespaces = many whitespace

skipSpaces :: Parser Unit
skipSpaces = skip whitespaces

-- | Parse a natural number amount of a given `Char` parser, resulting in a
-- | `String`.
manyChar :: Parser Char -> Parser String
manyChar p = fromCharList <$> many p

-- | Parse a positive integral amount of a given `Char` parser, resulting in a
-- | `String`.
someChar :: Parser Char -> Parser String
someChar p = fromCharList <$> some p

-- | Contiguous strings with no tabs, spaces, carriage returns or newlines.
word :: Parser String
word = fail "Expected contiguous string of nonwhitespace"
    |> someChar (sat \ c -> c /= ' ' && c /= '\t' && c /= '\r' && c /= '\n')

-- | Parse the end of a file, returning `Unit` to indicate success.
eof :: Parser Unit
eof = Parser \ str ->
  case str of
       "" -> Result (Right unit) str
       other ->
         let msg = "Expected empty string but found "
                <> show other
                <> " when trying to parse the string "
                <> show (take 5 str)
                <> "..."
          in Result (Left msg) str

-- | Parse an integer value as a `String`. Useful if needing to parse integers
-- | that are outside of `Int`'s bounds. You could then combine this with, e.g.
-- | `purescript-hugenum`'s `Data.HugeInt.fromString`.
integral :: Parser String
integral = do
  first <- digit <| char '-'
  digits <- many digit
  pure $ fromCharList (first : digits)

-- | Parse an `Int`. Note that this parser will fail if the candidate would be
-- | outside the range of allowable `Int`s.
int :: Parser Int
int = do
  n <- integral
  case fromString n of
       Just x -> pure x
       _ -> fail $ "Expected an int but found " <> show n

-- | Parse a `Number`. The string must have digits surrounding a decimal point.
number :: Parser Number
number = fail "Expected a number" |> do
  intPart <- integral
  char '.'
  fracPart <- manyChar digit
  pure $ readFloat $ intPart <> "." <> fracPart

boolean :: Parser Boolean
boolean =
  true <$ string "true" <| false <$ string "false" <?> "Expected a boolean"


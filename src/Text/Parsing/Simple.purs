module Text.Parsing.Simple
  -- definitional exports
  ( Parser(), ParseError(), parse
  -- utility functions
  , altL, (<|<)
  , altR, (>|>)
  , applyL, (<<)
  , applyR, (>>)
  , fromCharList
  -- polymorphic Parser-specific combinators
  , none
  , fail, orFailWith, (<?>)
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
  , sat, sat'
  , isn'tAnyF, isn'tAnyF', isn'tAny, isn'tAny'
  , anyOfF, anyOfF', anyOf, anyOf'
  , char, char', string, string'
  , tail
  , digit, digit', lower, lower', upper, upper', letter, letter', alphanum, alphanum'
  , space, space', tab, tab', newline, newline', cr, cr'
  , whitespace, whitespace', whitespaces, skipSpaces
  , word
  , eof
  , int, number, boolean
  ) where

import Prelude
import Global (readFloat)

import Control.Alt (class Alt, (<|>))
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

type ParseError = String
newtype Parser a = Parser (String -> { consumed :: Either ParseError a
                                     , remaining :: String
                                     })

-- | Unwraps the `newtype`, giving you a function which takes a `String` and
-- | returns a product of already-parsed data and the remaining `String`.
runParser :: forall a. Parser a -> String -> { consumed :: Either ParseError a, remaining :: String }
runParser (Parser x) = x

-- | Run a parser against a `String`, either getting an error or a value.
parse :: forall a. Parser a -> String -> Either ParseError a
parse (Parser p) input = (p input).consumed

instance semigroupParser :: Semigroup (Parser a) where
  append (Parser x) (Parser y) = Parser \ str ->
    let z = x str
     in case z.consumed of
             Right _ -> z
             _ -> y z.remaining

instance monoidParser :: Monoid (Parser a) where
  mempty = none

instance lazyParser :: Lazy (Parser a) where
  defer f = Parser \ str -> runParser (f unit) str

instance functorParser :: Functor Parser where
  map f (Parser p) = Parser \ str ->
    let x = p str
     in x { consumed = f <$> x.consumed }

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
  pure x = Parser \ str -> { consumed: Right x, remaining: str }

instance bindParser :: Bind Parser where
  bind (Parser mx) mf = Parser \ str ->
    let x = mx str
     in case x.consumed of
             Right y -> runParser (mf y) x.remaining
             _ ->
               let msg = "Parse failed at " <> take 5 str <> "..."
                in { consumed: Left msg, remaining: str }

instance monadParser :: Monad Parser

instance alternativeParser :: Alternative Parser

instance monadZeroParser :: MonadZero Parser

instance monadPlusParser :: MonadPlus Parser

altL :: forall a. Parser a -> Parser a -> Parser a
altL (Parser x) (Parser y) =
  Parser \ str -> let z = x str
                   in case z.consumed of
                       Right _ -> z
                       _ -> y str

infixl 3 altL as <|<

altR :: forall a. Parser a -> Parser a -> Parser a
altR (Parser x) (Parser y) =
  Parser \ str -> let z = y str
                   in case z.consumed of
                       Right _ -> z
                       _ -> x str

infixr 3 altR as >|>

-- | Equivalent to (<*) but faster since it doesn't require passing typeclass
-- | dictionaries.
applyL :: forall a b. Parser a -> Parser b -> Parser a
applyL (Parser f) (Parser g) = Parser \ str ->
  let x = f str
   in case x.consumed of
           t@(Right _) ->
             let y = g x.remaining
              in case y.consumed of
                      Right _ -> { consumed: t, remaining: y.remaining }
                      Left s -> { consumed: Left s, remaining: x.remaining }
           _ -> x

infixl 4 applyL as <<

-- | Equivalent to (*>) but faster since it doesn't require passing typeclass
-- | dictionaries.
applyR :: forall a b. Parser a -> Parser b -> Parser b
applyR (Parser f) (Parser g) = Parser \ str ->
  let x = f str
   in case x.consumed of
           (Right _) -> g x.remaining
           (Left s) -> { consumed: Left s, remaining: x.remaining }

infixl 4 applyR as >>

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = foldMap singleton

-- | Always fail.
none :: forall a. Parser a
none = Parser \ str -> { consumed: Left "Parse failed on `none`", remaining: str }

-- | Fail with a message
fail :: forall a. ParseError -> Parser a
fail msg = Parser \ str -> { consumed: Left msg, remaining: str }

orFailWith :: forall a. Parser a -> ParseError -> Parser a
orFailWith (Parser p) msg = Parser \ str ->
  let parsed = p str
   in case parsed.consumed of
           Right _ -> parsed
           _ -> { consumed: Left msg, remaining: str }

infix 0 orFailWith as <?>

-- | If the given parser fails, backtrack to the point of failure.
try :: forall a. Parser a -> Parser a
try (Parser x) = Parser \ str ->
  let y = x str
   in case y.consumed of
       Right _ -> y
       _ ->
        let msg = "Parse failed on `try`"
         in { consumed: Left msg, remaining: str }

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
               Right z -> go y.remaining f (z : acc)
               _ -> { consumed: Right (reverse acc), remaining: curr }

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
           Right _ ->
             let msg = "Parse failed on `isn't` when trying to parse the string "
                    <> show (take 5 str)
                    <> "..."
              in parsed { consumed = Left msg }
           _ -> parsed { consumed = Right unit }

-- | Differs from `isn't` in that this never consumes input.
notFollowedBy :: forall a. Parser a -> Parser Unit
notFollowedBy (Parser x) = Parser \ str ->
  let parsed = x str
   in case parsed.consumed of
           Right _ ->
             let msg = "Parse failed on `notFollowedBy` when trying to parse the string "
                    <> show (take 5 str)
                    <> "..."
              in { consumed: Left msg, remaining: str }
           _ -> { consumed: Right unit, remaining: str }

-- | Discard the result of a parse.
skip :: forall a. Parser a -> Parser Unit
skip (Parser p) =
  Parser \ str -> { consumed: Right unit, remaining: (p str).remaining }

-- | Attempt a parse subject to a predicate. If the parse succeeds but the
-- | predicate does not hold, the resulting parse fails *without* backtracking.
-- | If the parse fails, it will backtrack.
suchThat :: forall a. Parser a -> (a -> Boolean) -> Parser a
suchThat (Parser p) f = Parser \ str ->
  let parsed = p str
   in case parsed.consumed of
           Right res ->
             if f res
                then parsed
                else let msg = "Predicate failed on `suchThat` when trying to parse the string "
                            <> show (take 5 str)
                            <> "..."
                      in parsed { consumed = Left msg }
           _ -> { consumed: Left "Parse failed on `suchThat`", remaining: str }

infixl 5 suchThat as |=

-- | Parse a single `Char`.
item :: Parser Char
item = Parser \ str ->
  case charAt 0 str of
       Just c -> { consumed: Right c, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

-- | Create a parser from a `Char`acteristic function.
sat :: (Char -> Boolean) -> Parser Char
sat f = Parser \ str ->
  case charAt 0 str of
       Just c ->
         if f c
            then { consumed: Right c, remaining: drop 1 str }
            else let msg = "Character "
                        <> show c
                        <> " did not satisfy predicate when trying to parse the string "
                        <> show (take 5 str)
                        <> "..."
                  in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

sat' :: (Char -> Boolean) -> Parser Char
sat' f = Parser \ str ->
  case charAt 0 str of
       Just c ->
         if f c
            then { consumed: Right c, remaining: drop 1 str }
            else let msg = "Character "
                        <> show c
                        <> " did not satisfy predicate when trying to parse the string "
                        <> show (take 5 str)
                        <> "..."
                  in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

-- | Match any character not in the foldable container.
isn'tAnyF :: forall f. Foldable f => f Char -> Parser Char
isn'tAnyF xs = sat (_ `notElem` xs)

isn'tAnyF' :: forall f. Foldable f => f Char -> Parser Char
isn'tAnyF' xs = sat' (_ `notElem` xs)

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
                          in { consumed: Left msg, remaining: str }
                    else { consumed: Right c, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

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
                          in { consumed: Left msg, remaining: drop 1 str }
                    else { consumed: Right c, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

-- | Match any character in the foldable container.
anyOfF :: forall f. Foldable f => f Char -> Parser Char
anyOfF xs = sat (_ `elem` xs)

anyOfF' :: forall f. Foldable f => f Char -> Parser Char
anyOfF' xs = sat' (_ `elem` xs)

-- | Match any character in the string.
-- | Equivalent to `anyOfF <<< toCharArray`.
anyOf :: String -> Parser Char
anyOf s = Parser \ str ->
  case charAt 0 str of
       Just c -> if contains (singleton c) s
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected one of "
                                <> show s
                                <> " but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

anyOf' :: String -> Parser Char
anyOf' s = Parser \ str ->
  case charAt 0 str of
       Just c -> if contains (singleton c) s
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected one of "
                                <> show s
                                <> " but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

char :: Char -> Parser Char
char x = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == x
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected "
                                <> show x
                                <> " but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

char' :: Char -> Parser Char
char' x = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == x
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected "
                                <> show x
                                <> " but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

string :: String -> Parser String
string s = Parser \ str ->
  case indexOf s str of
       Just 0 -> { consumed: Right s, remaining: drop (length s) str }
       _ -> let msg = "Expecting "
                   <> show s
                   <> " but found "
                   <> show (take (length s) str)
             in { consumed: Left msg, remaining: str }

string' :: String -> Parser String
string' s = Parser \ str ->
  case indexOf s str of
       Just 0 -> { consumed: Right s, remaining: drop (length s) str }
       _ -> let msg = "Expecting "
                   <> show s
                   <> " but found "
                   <> show (take (length s) str)
             in { consumed: Left msg, remaining: drop (length s) str }

-- | Matches the unparsed portion of the input.
tail :: Parser String
tail = Parser \ str -> { consumed: Right str, remaining: "" }

digit :: Parser Char
digit = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= '0' && c <= '9'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a digit but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

digit' :: Parser Char
digit' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= '0' && c <= '9'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a digit but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

-- | Parse a lowercase character.
lower :: Parser Char
lower = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'a' && c <= 'z'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a lowercase character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

lower' :: Parser Char
lower' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'a' && c <= 'z'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a lowercase character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

-- | Parse an uppercase character.
upper :: Parser Char
upper = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected an uppercase character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

upper' :: Parser Char
upper' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected an uppercase character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

letter :: Parser Char
letter = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a letter but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

letter' :: Parser Char
letter' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a letter but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

-- | Parse a letter or a digit.
alphanum :: Parser Char
alphanum = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected alphanumeric character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

alphanum' :: Parser Char
alphanum' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected alphanumeric character but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

space :: Parser Char
space = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == ' '
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a space but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

space' :: Parser Char
space' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == ' '
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a space but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

tab :: Parser Char
tab = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\t'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a tab but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

tab' :: Parser Char
tab' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\t'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a tab but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

newline :: Parser Char
newline = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\n'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a newline but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

newline' :: Parser Char
newline' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\n'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected a newline but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

-- | Parse a carriage return.
cr :: Parser Char
cr = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\r'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected carriage return but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

cr' :: Parser Char
cr' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\r'
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected carriage return but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

whitespace :: Parser Char
whitespace = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\r' || c == '\n' || c == '\t' || c == ' '
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected whitespace but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

whitespace' :: Parser Char
whitespace' = Parser \ str ->
  case charAt 0 str of
       Just c -> if c == '\r' || c == '\n' || c == '\t' || c == ' '
                    then { consumed: Right c, remaining: drop 1 str }
                    else let msg = "Expected whitespace but found "
                                <> show c
                                <> " when trying to parse the string "
                                <> show (take 5 str)
                                <> "..."
                          in { consumed: Left msg, remaining: drop 1 str }
       _ -> { consumed: Left "Reached end of file", remaining: str }

whitespaces :: Parser (List Char)
whitespaces = many whitespace

skipSpaces :: Parser Unit
skipSpaces = skip whitespaces

-- | Contiguous strings with no tabs, spaces, carriage returns or newlines.
word :: Parser String
word = fail "Expected contiguous string of nonwhitespace"
   >|> fromCharList
   <$> some (sat \ c -> c /= ' ' && c /= '\t' && c /= '\r' && c /= '\n')

-- | Parse the end of a file, returning `Unit` to indicate success.
eof :: Parser Unit
eof = Parser \ str ->
  case str of
       "" -> { consumed: Right unit, remaining: str }
       other ->
         let msg = "Expected empty string but found "
                <> show other
                <> " when trying to parse the string "
                <> show (take 5 str)
                <> "..."
          in { consumed: Left msg, remaining: str }

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
       _ -> fail $ "Expected an int but found " <> show n

-- | Parse a `Number`. The string must have digits surrounding a decimal point.
number :: Parser Number
number = fail "Expected a number" >|> do
  integral <- numerals
  char '.'
  fractional <- fromCharList <$> many digit
  pure $ readFloat $ integral <> "." <> fractional

boolean :: Parser Boolean
boolean = fail "Expected a boolean" >|> do
  x <- string "true" <|> string "false"
  pure case x of
            "true" -> true
            _ -> false

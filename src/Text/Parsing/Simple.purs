module Text.Parsing.Simple where

import Prelude

import Control.Alt (class Alt, alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Monad.Transformerless.Except (Except, runExcept)
import Control.Monad.Transformerless.State (State, get, put, runState)
import Control.Plus (class Plus)
import Data.Char.Unicode (isAlphaNum, isDigit, isSpace)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, elem, foldl, notElem)
import Data.Int (round)
import Data.List (List(..), many, singleton, some, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple, fst)
import Global (readInt)
import Text.Parsing.Util (fromChars, lift, throwState, throwStateList)

type ParseError = String

type Result o = Except (List ParseError) o

newtype Parser i o = Parser (State i (Result o))

derive instance newtypeParser :: Newtype (Parser i o) _

unParser :: forall i o. Parser i o -> State i (Result o)
unParser (Parser s) = s

runParser :: forall i o. Parser i o -> i -> Tuple (Result o) i
runParser p = runState (unParser p)

parse :: forall i o. Parser i o -> i -> Result o
parse p i = fst (runParser p i)

derive instance functorParser :: Functor (Parser i)

instance applyParser :: Apply (Parser i) where
  apply pf pa = Parser do
    st <- get
    rf <- unParser pf
    case runExcept rf of
      Right f -> do
        st' <- get
        ra <- unParser pa
        case runExcept ra of
          Right a -> lift (f a)
          Left e -> do
            put st'
            throwState e
      Left e -> do
        put st
        throwState e

instance applicativeParser :: Applicative (Parser i) where
  pure a = Parser (lift a)

instance bindParser :: Bind (Parser i) where
  bind pa k = Parser do
    st <- get
    ra <- unParser pa
    case runExcept ra of
      Right a -> unParser (k a)
      Left e -> do
        put st
        throwState e

instance monadParser :: Monad (Parser i)

instance lazyParser :: Lazy (Parser i o) where
  defer f = Parser (defer (unwrap <<< f))

instance semigroupParser :: Semigroup o => Semigroup (Parser i o) where
  append = lift2 append

instance monoidParser :: Monoid o => Monoid (Parser i o) where
  mempty = pure mempty

instance altParser :: Alt (Parser i) where
  alt p1 p2 = Parser (alt <$> unParser p1 <*> unParser p2)

instance plusParser :: Plus (Parser i) where
  empty = fail "No alternative"

instance alternativeParser :: Alternative (Parser i)

stream :: forall i. Parser i i
stream = Parser (pure <$> get)

fail :: forall i o. ParseError -> Parser i o
fail = Parser <<< throwStateList

orFailWith :: forall i o. Parser i o -> ParseError -> Parser i o
orFailWith p m = Parser do
  p' <- unParser p
  case runExcept p' of
    Left _ -> throwStateList m
    Right a -> lift a

infix 0 orFailWith as <?>

bracket :: forall i o open close. Parser i open -> Parser i close -> Parser i o -> Parser i o
bracket open close a = open *> a <* close

option :: forall i o. o -> Parser i o -> Parser i o
option def p = p <|> pure def

optional :: forall i o. Parser i o -> Parser i Unit
optional p = void p <|> pure unit

optionMaybe :: forall i o. Parser i o -> Parser i (Maybe o)
optionMaybe p = option Nothing (Just <$> p)

try :: forall i o. Parser i o -> Parser i o
try p = Parser do
  st <- get
  res <- unParser p
  case runExcept res of
    Left e -> do
      put st
      pure res
    _ -> pure res

lookAhead :: forall i o. Parser i o -> Parser i o
lookAhead p = Parser do
  st <- get
  res <- unParser p
  put st
  pure res

sepBy :: forall i sep o. Parser i o -> Parser i sep -> Parser i (List o)
sepBy p sep = sepBy1 p sep <|> pure Nil

sepBy1 :: forall i sep o. Parser i o -> Parser i sep -> Parser i (List o)
sepBy1 p sep = do
  a <- p
  as <- many (sep *> p)
  pure (a : as)

endBy :: forall i o sep. Parser i o -> Parser i sep -> Parser i (List o)
endBy p sep = many $ p <* sep

endBy1 :: forall i o sep. Parser i o -> Parser i sep -> Parser i (List o)
endBy1 p sep = some $ p <* sep

sepEndBy :: forall i o sep. Parser i o -> Parser i sep -> Parser i (List o)
sepEndBy p sep = sepEndBy1 p sep <|> pure Nil

sepEndBy1 :: forall i o sep. Parser i o -> Parser i sep -> Parser i (List o)
sepEndBy1 p sep = do
  a <- p
  (do
    _ <- sep
    as <- sepEndBy p sep
    pure (a : as)) <|> pure (singleton a)

chainr :: forall i o. Parser i o -> Parser i (o -> o -> o) -> o -> Parser i o
chainr p f a = chainr1 p f <|> pure a

chainr1 :: forall i o. Parser i o -> Parser i (o -> o -> o) -> Parser i o
chainr1 p f = do
  a <- p
  chainr1' p f a

chainr1' :: forall i o. Parser i o -> Parser i (o -> o -> o) -> o -> Parser i o
chainr1' p f a =
  (do f' <- f
      a' <- chainr1 p f
      pure $ f' a a') <|> pure a

choice :: forall f g a. Foldable f => Alt g => Plus g => f (g a) -> g a
choice = foldl alt empty

skipMany :: forall i o. Parser i o -> Parser i Unit
skipMany p = skipSome p <|> pure unit

skipSome :: forall i o. Parser i o -> Parser i Unit
skipSome p = do
  _ <- p
  skipMany p

notFollowedBy :: forall i o. Parser i o -> Parser i Unit
notFollowedBy p = try ((try p *> fail "Negated parser succeeded") <|> pure unit)

manyTill :: forall i o e. Parser i o -> Parser i e -> Parser i (List o)
manyTill p e = go where
  go = (e $> Nil) <|> do
    x <- p
    xs <- go
    pure (x : xs)

someTill :: forall i o e. Parser i o -> Parser i e -> Parser i (List o)
someTill p e = do
  x <- p
  xs <- manyTill p e
  pure (x : xs)

suchThat :: forall i o. Parser i o -> (o -> Boolean) -> Parser i o
suchThat p pred = do
  x <- p
  if pred x
    then pure x
    else fail "Predicate failed"

infixl 5 suchThat as |=

anyChar :: Parser String Char
anyChar = Parser do
  st <- get
  case String.uncons st of
    Just {head, tail} -> do
      put tail
      pure (pure head)
    _ -> throwStateList "Unexpected EOF"

char :: Char -> Parser String Char
char c = satisfy (eq c) <?> "Expecting " <> show c

satisfy :: (Char -> Boolean) -> Parser String Char
satisfy p = anyChar |= p <?> "Character failed predicate"

string :: String -> Parser String String
string s = Parser do
  st <- get
  let l = String.length s
  if String.take l st == s
    then do
      put (String.drop l st)
      pure (pure s)
    else throwStateList ("Expected " <> show s)

space :: Parser String Char
space = satisfy isSpace

whiteSpace :: Parser String String
whiteSpace = fromChars <$> many space

skipSpaces :: Parser String Unit
skipSpaces = void whiteSpace

oneOf :: forall f. Foldable f => Show (f Char) => f Char -> Parser String Char
oneOf cs = satisfy (_ `elem` cs) <?> "one of " <> show cs

noneOf :: forall f. Foldable f => Show (f Char) => f Char -> Parser String Char
noneOf cs = satisfy (_ `notElem` cs) <?> "none of " <> show cs

eof :: Parser String Unit
eof = Parser do
  st <- get
  if st == ""
    then pure (pure unit)
    else throwStateList "Expected EOF"

someChar :: Parser String Char -> Parser String String
someChar p = fromChars <$> some p

integral :: Parser String String
integral = someChar (satisfy isDigit)

int :: Parser String Int
int = round <<< readInt 10 <$> integral

alphaNum :: Parser String Char
alphaNum = satisfy isAlphaNum

float :: Parser String Number
float = do
  i <- integral
  c <- char '.'
  f <- integral
  pure (readInt 10 (i <> String.singleton c <> f))

word :: Parser String String
word = someChar (satisfy (not <<< isSpace))
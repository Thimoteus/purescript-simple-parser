module Text.Parsing.Combinators
  ( choice
  , option
  , bracket
  , many
  , many1
  , exactly
  , atLeast
  , atMost
  , sepBy
  , sepBy1
  , sepEndBy
  ) where

import Prelude (class Applicative, pure, bind, append, (<$>), (<*>), (-))

import Data.Foldable (class Foldable, foldl)
import Data.List (List(..), (:))

import Control.Plus (class Plus, empty)
import Control.Alt (alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply ((<*), (*>))
import Control.MonadPlus (class MonadPlus)

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
many p = many1 p <|> pure Nil

-- | Parse at least once, giving a `List`.
many1 :: forall m a. MonadPlus m => m a -> m (List a)
many1 p = do
  a <- p
  as <- many p
  pure (a : as)

-- | `exactly n p` fails iff `many p` would produce a list of length < n.
-- | However, `exactly n p` will never produce a list of *larger* length.
-- | Example in PSCI:
-- | ```purescript
-- | > parse (exactly 3 $ char '6') "6666666"
-- | Just (Cons ('6') (Cons ('6') (Cons ('6'))))
-- | > parse (exactly 3 $ string "Bye) "ByeBye"
-- | Nothing
-- | ```
exactly :: forall m a. Applicative m => Int -> m a -> m (List a)
exactly = go (pure Nil) where
  go acc 0 _ = acc
  go acc n p = go (Cons <$> p <*> acc) (n - 1) p

-- | `atLeast n p` fails iff `exactly n p` does, but it differs in that there is
-- | no upper bound on the length of the produced list.
atLeast :: forall m a. MonadPlus m => Int -> m a -> m (List a)
atLeast n p = append <$> exactly n p <*> many p

-- | `atMost n p` never fails and never produces a list of length larger than n.
atMost :: forall m a. MonadPlus m => Int -> m a -> m (List a)
atMost n p = go (exactly n p) n p where
  go acc 0 _ = acc
  go acc n p = go (acc <|> exactly (n-1) p) (n - 1) p

-- | Given a value to parse and a separating parser, put all the values it finds
-- | into a `List`.
-- | Example:
-- | ```purescript
-- | > parse (int `sepBy` char ',' <* space) "123,456,789 years ago"
-- | Just (Cons (123) (Cons (456) (Cons (789) (Nil))))
-- | ```
sepBy :: forall m a b. MonadPlus m => m a -> m b -> m (List a)
sepBy target separator = sepBy1 target separator <|> pure Nil

-- | Parse one or more occurrences, separated by a delimiter.
sepBy1 :: forall m a b. MonadPlus m => m a -> m b -> m (List a)
sepBy1 target separator = Cons <$> target <*> many (separator *> target)

-- | Does the same as `sepBy`, but requires the separating parser to appear at
-- | the end.
sepEndBy :: forall m a b. MonadPlus m => m a -> m b -> m (List a)
sepEndBy target separator = sepBy target separator <* separator

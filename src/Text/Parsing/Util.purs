module Text.Parsing.Util where

import Prelude

import Control.Monad.Transformerless.Except (Except, throwError)
import Control.Monad.Transformerless.State (State)
import Data.Foldable (class Foldable, foldMap)
import Data.List (List, singleton)
import Data.String.CodeUnits as S

fromChars :: forall f. Foldable f => f Char -> String
fromChars = foldMap S.singleton

liftF :: forall f a b. Applicative f => (a -> b) -> a -> f b
liftF f a = pure (f a)

lift :: forall f g a. Applicative f => Applicative g => a -> f (g a)
lift = liftF pure

throwStateList :: forall s e a. e -> State s (Except (List e) a)
throwStateList = liftF (throwError <<< singleton)

throwState :: forall s e a. e -> State s (Except e a)
throwState = liftF throwError
## Module Text.Parsing.Simple

#### `ParseError`

``` purescript
type ParseError = String
```

#### `Parser`

``` purescript
newtype Parser s a
```

##### Instances
``` purescript
Semigroup (Parser s a)
Monoid (Parser s a)
Lazy (Parser s a)
Functor (Parser s)
Alt (Parser s)
Plus (Parser s)
Apply (Parser s)
Applicative (Parser s)
Bind (Parser s)
Monad (Parser s)
Alternative (Parser s)
MonadZero (Parser s)
MonadPlus (Parser s)
```

#### `parse`

``` purescript
parse :: forall s a. Parser s a -> s -> Either ParseError a
```

Run a parser against an input, either getting an error or a value.

#### `unparser`

``` purescript
unparser :: forall s a. Parser s a -> s -> { consumed :: Either ParseError a, remaining :: s }
```

Get the result of a parse, plus the unparsed input remainder.

#### `modify`

``` purescript
modify :: forall s a. (s -> s) -> Parser s a -> Parser s a
```

Change the input to a parser.

#### `modifyM`

``` purescript
modifyM :: forall s a. (s -> Maybe s) -> Parser s a -> Parser s a
```

Change the input to a parser, using `Nothing` to signal failure.

#### `modifyE`

``` purescript
modifyE :: forall s err a. Show err => (s -> Either err s) -> Parser s a -> Parser s a
```

Change the input to a parser, using `Left` to signal failure.

#### `pureP`

``` purescript
pureP :: forall s a. a -> Parser s a
```

A `pure` that doesn't require passing the typeclass dictionary for `Applicative`.

#### `mapP`

``` purescript
mapP :: forall s a b. (a -> b) -> Parser s a -> Parser s b
```

A `map` that doesn't require passing the typeclass dictionary for `Functor`.

#### `(|->)`

``` purescript
infixl 4 mapP as |->
```

#### `applyP`

``` purescript
applyP :: forall s a b. Parser s (a -> b) -> Parser s a -> Parser s b
```

An `apply` that doesn't require passing the typeclass dictionary for `Apply`.

#### `(~)`

``` purescript
infixl 4 applyP as ~
```

#### `bindP`

``` purescript
bindP :: forall s a b. Parser s a -> (a -> Parser s b) -> Parser s b
```

A `bind` that doesn't require passing the typeclass dictionary for `Bind`.

#### `(>>-)`

``` purescript
infixl 1 bindP as >>-
```

#### `flippedBindP`

``` purescript
flippedBindP :: forall s a b. (a -> Parser s b) -> Parser s a -> Parser s b
```

#### `(-<<)`

``` purescript
infixr 1 flippedBindP as -<<
```

#### `composeKleisliParser`

``` purescript
composeKleisliParser :: forall s a b c. (b -> Parser s c) -> (a -> Parser s b) -> (a -> Parser s c)
```

#### `(<-<)`

``` purescript
infixr 1 composeKleisliParser as <-<
```

#### `parserKleisliCompose`

``` purescript
parserKleisliCompose :: forall s a b c. (a -> Parser s b) -> (b -> Parser s c) -> (a -> Parser s c)
```

#### `(>->)`

``` purescript
infixr 1 parserKleisliCompose as >->
```

#### `altL`

``` purescript
altL :: forall s a. Parser s a -> Parser s a -> Parser s a
```

#### `(<|)`

``` purescript
infixl 3 altL as <|
```

#### `altR`

``` purescript
altR :: forall s a. Parser s a -> Parser s a -> Parser s a
```

#### `(|>)`

``` purescript
infixr 3 altR as |>
```

#### `applyL`

``` purescript
applyL :: forall s a b. Parser s a -> Parser s b -> Parser s a
```

Equivalent to (<*) but faster since it doesn't require passing typeclass
dictionaries.

#### `(<<)`

``` purescript
infixl 4 applyL as <<
```

#### `applyR`

``` purescript
applyR :: forall s a b. Parser s a -> Parser s b -> Parser s b
```

Equivalent to (*>) but faster since it doesn't require passing typeclass
dictionaries.

#### `(>>)`

``` purescript
infixl 4 applyR as >>
```

#### `fromCharList`

``` purescript
fromCharList :: forall f. Foldable f => f Char -> String
```

#### `none`

``` purescript
none :: forall s a. Parser s a
```

Always fail.

#### `fail`

``` purescript
fail :: forall s a. ParseError -> Parser s a
```

Fail with a message

#### `orFailWith`

``` purescript
orFailWith :: forall s a. Parser s a -> ParseError -> Parser s a
```

#### `(<?>)`

``` purescript
infix 0 orFailWith as <?>
```

#### `try`

``` purescript
try :: forall s a. Parser s a -> Parser s a
```

If the given parser fails, backtrack to the point of failure.

#### `many`

``` purescript
many :: forall s a. Parser s a -> Parser s (List a)
```

Attempt a parse as many times as possible, putting all successes into
a list.
WARNING: Applying this to a parser which never fails and never consumes
input will result in a bottom, i.e. a nonterminating program.

#### `some`

``` purescript
some :: forall s a. Parser s a -> Parser s (List a)
```

Attempt a parse one or more times.

#### `fix`

``` purescript
fix :: forall s a. (Parser s a -> Parser s a) -> Parser s a
```

Find a function's least fixed point using the Z combinator.

#### `lookahead`

``` purescript
lookahead :: forall s a. Parser s a -> Parser s a
```

Parse without consuming input.

#### `isn't`

``` purescript
isn't :: forall s a. Show s => Parser s a -> Parser s Unit
```

`isn't p` succeeds iff p fails, though it will always consume the same
amount of string that p does.

#### `notFollowedBy`

``` purescript
notFollowedBy :: forall s a. Show s => Parser s a -> Parser s Unit
```

Differs from `isn't` in that this never consumes input.

#### `skip`

``` purescript
skip :: forall s a. Parser s a -> Parser s Unit
```

Discard the result of a parse.

#### `suchThat`

``` purescript
suchThat :: forall s a. Show s => Parser s a -> (a -> Boolean) -> Parser s a
```

Attempt a parse subject to a predicate. If the parse succeeds but the
predicate does not hold, the resulting parse fails *without* backtracking.
If the parse fails, it will backtrack.

#### `(|=)`

``` purescript
infixl 5 suchThat as |=
```

#### `sepBy`

``` purescript
sepBy :: forall s a b. Parser s a -> Parser s b -> Parser s (List a)
```

#### `sepBy1`

``` purescript
sepBy1 :: forall s a b. Parser s a -> Parser s b -> Parser s (List a)
```

#### `tail`

``` purescript
tail :: Parser String String
```

Matches the unparsed portion of the input.

#### `item`

``` purescript
item :: Parser String Char
```

Parse a single `Char`.

#### `first`

``` purescript
first :: forall s a. (s -> Maybe { head :: a, tail :: s }) -> Parser s a
```

A generalized `item` for arbitrary streams that can be `uncons`ed.

#### `sat`

``` purescript
sat :: (Char -> Boolean) -> Parser String Char
```

## Backtracking combinators
Create a parser from a `Char`acteristic function.

#### `isn'tAnyF`

``` purescript
isn'tAnyF :: forall f. Foldable f => f Char -> Parser String Char
```

Match any character not in the foldable container.

#### `isn'tAny`

``` purescript
isn'tAny :: String -> Parser String Char
```

Match any character not in the string.
Equivalent to `isn'tAnyF <<< toCharArray`.

#### `anyOfF`

``` purescript
anyOfF :: forall f. Foldable f => f Char -> Parser String Char
```

Match any character in the foldable container.

#### `anyOf`

``` purescript
anyOf :: String -> Parser String Char
```

Match any character in the string.
Equivalent to `anyOfF <<< toCharArray`.

#### `char`

``` purescript
char :: Char -> Parser String Char
```

#### `string`

``` purescript
string :: String -> Parser String String
```

#### `digit`

``` purescript
digit :: Parser String Char
```

#### `lower`

``` purescript
lower :: Parser String Char
```

Parse a lowercase character.

#### `upper`

``` purescript
upper :: Parser String Char
```

Parse an uppercase character.

#### `letter`

``` purescript
letter :: Parser String Char
```

#### `alphanum`

``` purescript
alphanum :: Parser String Char
```

Parse a letter or a digit.

#### `space`

``` purescript
space :: Parser String Char
```

#### `tab`

``` purescript
tab :: Parser String Char
```

#### `newline`

``` purescript
newline :: Parser String Char
```

#### `cr`

``` purescript
cr :: Parser String Char
```

Parse a carriage return.

#### `whitespace`

``` purescript
whitespace :: Parser String Char
```

#### `sat'`

``` purescript
sat' :: (Char -> Boolean) -> Parser String Char
```

## Non-backtracking combinators

#### `isn'tAnyF'`

``` purescript
isn'tAnyF' :: forall f. Foldable f => f Char -> Parser String Char
```

#### `isn'tAny'`

``` purescript
isn'tAny' :: String -> Parser String Char
```

#### `anyOfF'`

``` purescript
anyOfF' :: forall f. Foldable f => f Char -> Parser String Char
```

#### `anyOf'`

``` purescript
anyOf' :: String -> Parser String Char
```

#### `char'`

``` purescript
char' :: Char -> Parser String Char
```

#### `string'`

``` purescript
string' :: String -> Parser String String
```

#### `digit'`

``` purescript
digit' :: Parser String Char
```

#### `lower'`

``` purescript
lower' :: Parser String Char
```

#### `upper'`

``` purescript
upper' :: Parser String Char
```

#### `letter'`

``` purescript
letter' :: Parser String Char
```

#### `alphanum'`

``` purescript
alphanum' :: Parser String Char
```

#### `space'`

``` purescript
space' :: Parser String Char
```

#### `tab'`

``` purescript
tab' :: Parser String Char
```

#### `newline'`

``` purescript
newline' :: Parser String Char
```

#### `cr'`

``` purescript
cr' :: Parser String Char
```

#### `whitespace'`

``` purescript
whitespace' :: Parser String Char
```

#### `whitespaces`

``` purescript
whitespaces :: Parser String (List Char)
```

## Higher-order combinators

#### `skipSpaces`

``` purescript
skipSpaces :: Parser String Unit
```

#### `manyChar`

``` purescript
manyChar :: Parser String Char -> Parser String String
```

Parse a natural number amount of a given `Char` parser, resulting in a
`String`.

#### `someChar`

``` purescript
someChar :: Parser String Char -> Parser String String
```

Parse a positive integral amount of a given `Char` parser, resulting in a
`String`.

#### `word`

``` purescript
word :: Parser String String
```

Contiguous strings with no tabs, spaces, carriage returns or newlines.

#### `eof`

``` purescript
eof :: Parser String Unit
```

Parse the end of a file, returning `Unit` to indicate success.

#### `integral`

``` purescript
integral :: Parser String String
```

Parse an integer value as a `String`. Useful if needing to parse integers
that are outside of `Int`'s bounds. You could then combine this with, e.g.
`purescript-hugenum`'s `Data.HugeInt.fromString`.

#### `int`

``` purescript
int :: Parser String Int
```

Parse an `Int`. Note that this parser will fail if the candidate would be
outside the range of allowable `Int`s.

#### `number`

``` purescript
number :: Parser String Number
```

Parse a `Number`. The string must have digits surrounding a decimal point.

#### `boolean`

``` purescript
boolean :: Parser String Boolean
```

#### `parens`

``` purescript
parens :: forall a. Parser String a -> Parser String a
```

#### `braces`

``` purescript
braces :: forall a. Parser String a -> Parser String a
```

#### `brackets`

``` purescript
brackets :: forall a. Parser String a -> Parser String a
```



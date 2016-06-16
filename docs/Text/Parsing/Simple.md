## Module Text.Parsing.Simple

#### `ParseError`

``` purescript
type ParseError = String
```

#### `Parser`

``` purescript
newtype Parser a
```

##### Instances
``` purescript
Semigroup (Parser a)
Monoid (Parser a)
Lazy (Parser a)
Functor Parser
Alt Parser
Plus Parser
Apply Parser
Applicative Parser
Bind Parser
Monad Parser
Alternative Parser
MonadZero Parser
MonadPlus Parser
```

#### `parse`

``` purescript
parse :: forall a. Parser a -> String -> Either ParseError a
```

Run a parser against a `String`, either getting an error or a value.

#### `altL`

``` purescript
altL :: forall a. Parser a -> Parser a -> Parser a
```

#### `(<|)`

``` purescript
infixl 3 altL as <|
```

#### `altR`

``` purescript
altR :: forall a. Parser a -> Parser a -> Parser a
```

#### `(|>)`

``` purescript
infixr 3 altR as |>
```

#### `applyL`

``` purescript
applyL :: forall a b. Parser a -> Parser b -> Parser a
```

Equivalent to (<*) but faster since it doesn't require passing typeclass
dictionaries.

#### `(<<)`

``` purescript
infixl 4 applyL as <<
```

#### `applyR`

``` purescript
applyR :: forall a b. Parser a -> Parser b -> Parser b
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
none :: forall a. Parser a
```

Always fail.

#### `fail`

``` purescript
fail :: forall a. ParseError -> Parser a
```

Fail with a message

#### `orFailWith`

``` purescript
orFailWith :: forall a. Parser a -> ParseError -> Parser a
```

#### `(<?>)`

``` purescript
infix 0 orFailWith as <?>
```

#### `try`

``` purescript
try :: forall a. Parser a -> Parser a
```

If the given parser fails, backtrack to the point of failure.

#### `many`

``` purescript
many :: forall a. Parser a -> Parser (List a)
```

Attempt a parse as many times as possible, putting all successes into
a list.
WARNING: Applying this to a parser which never fails and never consumes
input will result in a bottom, i.e. a nonterminating program.

#### `some`

``` purescript
some :: forall a. Parser a -> Parser (List a)
```

Attempt a parse one or more times.

#### `fix`

``` purescript
fix :: forall a. (Parser a -> Parser a) -> Parser a
```

Find a function's least fixed point.

#### `lookahead`

``` purescript
lookahead :: forall a. Parser a -> Parser a
```

Parse without consuming input.

#### `isn't`

``` purescript
isn't :: forall a. Parser a -> Parser Unit
```

`isn't p` succeeds iff p fails, though it will always consume the same
amount of string that p does.

#### `notFollowedBy`

``` purescript
notFollowedBy :: forall a. Parser a -> Parser Unit
```

Differs from `isn't` in that this never consumes input.

#### `skip`

``` purescript
skip :: forall a. Parser a -> Parser Unit
```

Discard the result of a parse.

#### `suchThat`

``` purescript
suchThat :: forall a. Parser a -> (a -> Boolean) -> Parser a
```

Attempt a parse subject to a predicate. If the parse succeeds but the
predicate does not hold, the resulting parse fails *without* backtracking.
If the parse fails, it will backtrack.

#### `(|=)`

``` purescript
infixl 5 suchThat as |=
```

#### `item`

``` purescript
item :: Parser Char
```

Parse a single `Char`.

#### `sat`

``` purescript
sat :: (Char -> Boolean) -> Parser Char
```

Create a parser from a `Char`acteristic function.

#### `sat'`

``` purescript
sat' :: (Char -> Boolean) -> Parser Char
```

#### `isn'tAnyF`

``` purescript
isn'tAnyF :: forall f. Foldable f => f Char -> Parser Char
```

Match any character not in the foldable container.

#### `isn'tAnyF'`

``` purescript
isn'tAnyF' :: forall f. Foldable f => f Char -> Parser Char
```

#### `isn'tAny`

``` purescript
isn'tAny :: String -> Parser Char
```

Match any character not in the string.
Equivalent to `isn'tAnyF <<< toCharArray`.

#### `isn'tAny'`

``` purescript
isn'tAny' :: String -> Parser Char
```

#### `anyOfF`

``` purescript
anyOfF :: forall f. Foldable f => f Char -> Parser Char
```

Match any character in the foldable container.

#### `anyOfF'`

``` purescript
anyOfF' :: forall f. Foldable f => f Char -> Parser Char
```

#### `anyOf`

``` purescript
anyOf :: String -> Parser Char
```

Match any character in the string.
Equivalent to `anyOfF <<< toCharArray`.

#### `anyOf'`

``` purescript
anyOf' :: String -> Parser Char
```

#### `char`

``` purescript
char :: Char -> Parser Char
```

#### `char'`

``` purescript
char' :: Char -> Parser Char
```

#### `string`

``` purescript
string :: String -> Parser String
```

#### `string'`

``` purescript
string' :: String -> Parser String
```

#### `tail`

``` purescript
tail :: Parser String
```

Matches the unparsed portion of the input.

#### `digit`

``` purescript
digit :: Parser Char
```

#### `digit'`

``` purescript
digit' :: Parser Char
```

#### `lower`

``` purescript
lower :: Parser Char
```

Parse a lowercase character.

#### `lower'`

``` purescript
lower' :: Parser Char
```

#### `upper`

``` purescript
upper :: Parser Char
```

Parse an uppercase character.

#### `upper'`

``` purescript
upper' :: Parser Char
```

#### `letter`

``` purescript
letter :: Parser Char
```

#### `letter'`

``` purescript
letter' :: Parser Char
```

#### `alphanum`

``` purescript
alphanum :: Parser Char
```

Parse a letter or a digit.

#### `alphanum'`

``` purescript
alphanum' :: Parser Char
```

#### `space`

``` purescript
space :: Parser Char
```

#### `space'`

``` purescript
space' :: Parser Char
```

#### `tab`

``` purescript
tab :: Parser Char
```

#### `tab'`

``` purescript
tab' :: Parser Char
```

#### `newline`

``` purescript
newline :: Parser Char
```

#### `newline'`

``` purescript
newline' :: Parser Char
```

#### `cr`

``` purescript
cr :: Parser Char
```

Parse a carriage return.

#### `cr'`

``` purescript
cr' :: Parser Char
```

#### `whitespace`

``` purescript
whitespace :: Parser Char
```

#### `whitespace'`

``` purescript
whitespace' :: Parser Char
```

#### `whitespaces`

``` purescript
whitespaces :: Parser (List Char)
```

#### `skipSpaces`

``` purescript
skipSpaces :: Parser Unit
```

#### `word`

``` purescript
word :: Parser String
```

Contiguous strings with no tabs, spaces, carriage returns or newlines.

#### `eof`

``` purescript
eof :: Parser Unit
```

Parse the end of a file, returning `Unit` to indicate success.

#### `int`

``` purescript
int :: Parser Int
```

Parse an `Int`. Note that this parser will fail if the candidate would be
outside the range of allowable `Int`s.

#### `number`

``` purescript
number :: Parser Number
```

Parse a `Number`. The string must have digits surrounding a decimal point.

#### `boolean`

``` purescript
boolean :: Parser Boolean
```



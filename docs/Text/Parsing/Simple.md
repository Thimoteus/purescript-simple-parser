## Module Text.Parsing.Simple

#### `Parser`

``` purescript
newtype Parser a
```

##### Instances
``` purescript
Semigroup (Parser a)
Monoid (Parser a)
Functor Parser
Alt Parser
Plus Parser
Apply Parser
Applicative Parser
Bind Parser
Monad Parser
Alternative Parser
MonadPlus Parser
```

#### `runParser`

``` purescript
runParser :: forall a. Parser a -> String -> { consumed :: Maybe a, remaining :: String }
```

Unwraps the `newtype`, giving you a function which takes a `String` and
returns a product of already-parsed data and the remaining `String`.

#### `parse`

``` purescript
parse :: forall a. Parser a -> String -> Maybe a
```

Run a given parser against a `String`, maybe getting a value or nothing.

#### `choice`

``` purescript
choice :: forall f m a. (Plus m, Foldable f) => f (m a) -> m a
```

Choose the first successful element from a foldable container of parsers.

#### `option`

``` purescript
option :: forall m a. (Alternative m) => a -> m a -> m a
```

Attempt a parse, with a default value in case of failure.

#### `bracket`

``` purescript
bracket :: forall m l r a. (Applicative m) => m l -> m a -> m r -> m a
```

Parse something surrounded by given arguments.

#### `many`

``` purescript
many :: forall m a. (MonadPlus m) => m a -> m (List a)
```

Parse as many times as possible, giving a `List`.

#### `many1`

``` purescript
many1 :: forall m a. (MonadPlus m) => m a -> m (List a)
```

Parse at least once, giving a `List`.

#### `fromCharList`

``` purescript
fromCharList :: forall f. (Foldable f) => f Char -> String
```

#### `none`

``` purescript
none :: forall a. Parser a
```

Always fail.

#### `item`

``` purescript
item :: Parser Char
```

Parse a single `Char`.

#### `sepBy`

``` purescript
sepBy :: forall a b. Parser a -> Parser b -> Parser (List a)
```

Given a value to parse and a separating parser, put all the values it finds
into a `List`.

#### `sat`

``` purescript
sat :: (Char -> Boolean) -> Parser Char
```

Create a parser from a characteristic function.

#### `try`

``` purescript
try :: forall a. Parser a -> Parser a
```

If the given parser fails, return to the point of failure.

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

#### `notFollowedBy`

``` purescript
notFollowedBy :: forall a. Parser a -> Parser Unit
```

#### `skip`

``` purescript
skip :: forall a. Parser a -> Parser Unit
```

Discard the result of a parse.

#### `char`

``` purescript
char :: Char -> Parser Char
```

#### `string`

``` purescript
string :: String -> Parser String
```

#### `digit`

``` purescript
digit :: Parser Char
```

#### `lower`

``` purescript
lower :: Parser Char
```

Parse a lowercase character.

#### `upper`

``` purescript
upper :: Parser Char
```

Parse an uppercase character.

#### `letter`

``` purescript
letter :: Parser Char
```

#### `alphanum`

``` purescript
alphanum :: Parser Char
```

Parse a letter or a digit.

#### `space`

``` purescript
space :: Parser Char
```

#### `tab`

``` purescript
tab :: Parser Char
```

#### `newline`

``` purescript
newline :: Parser Char
```

#### `cr`

``` purescript
cr :: Parser Char
```

Parse a carriage return.

#### `whitespace`

``` purescript
whitespace :: Parser Char
```

#### `whitespaces`

``` purescript
whitespaces :: Parser (List Char)
```

#### `skipSpaces`

``` purescript
skipSpaces :: Parser Unit
```

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



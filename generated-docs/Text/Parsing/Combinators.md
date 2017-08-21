## Module Text.Parsing.Combinators

#### `choice`

``` purescript
choice :: forall f m a. Plus m => Foldable f => f (m a) -> m a
```

Choose the first successful element from a foldable container of parsers.

#### `option`

``` purescript
option :: forall m a. Alternative m => a -> m a -> m a
```

Attempt a parse, with a default value in case of failure.

#### `bracket`

``` purescript
bracket :: forall m l r a. Applicative m => m l -> m a -> m r -> m a
```

Parse something surrounded by given arguments.

#### `many`

``` purescript
many :: forall m a. MonadPlus m => m a -> m (List a)
```

Parse as many times as possible, giving a `List`.

#### `many1`

``` purescript
many1 :: forall m a. MonadPlus m => m a -> m (List a)
```

Parse at least once, giving a `List`.

#### `exactly`

``` purescript
exactly :: forall m a. Applicative m => Int -> m a -> m (List a)
```

`exactly n p` fails iff `many p` would produce a list of length < n.
However, `exactly n p` will never produce a list of *larger* length.
Example in PSCI:
```purescript
> parse (exactly 3 $ char '6') "6666666"
Just (Cons ('6') (Cons ('6') (Cons ('6'))))
> parse (exactly 3 $ string "Bye) "ByeBye"
Nothing
```

#### `atLeast`

``` purescript
atLeast :: forall m a. MonadPlus m => Int -> m a -> m (List a)
```

`atLeast n p` fails iff `exactly n p` does, but it differs in that there is
no upper bound on the length of the produced list.

#### `atMost`

``` purescript
atMost :: forall m a. MonadPlus m => Int -> m a -> m (List a)
```

`atMost n p` never fails and never produces a list of length larger than n.

#### `sepBy`

``` purescript
sepBy :: forall m a b. MonadPlus m => m a -> m b -> m (List a)
```

Given a value to parse and a separating parser, put all the values it finds
into a `List`.
Example:
```purescript
> parse (int `sepBy` char ',' <* space) "123,456,789 years ago"
Just (Cons (123) (Cons (456) (Cons (789) (Nil))))
```

#### `sepBy1`

``` purescript
sepBy1 :: forall m a b. MonadPlus m => m a -> m b -> m (List a)
```

Parse one or more occurrences, separated by a delimiter.

#### `sepEndBy`

``` purescript
sepEndBy :: forall m a b. MonadPlus m => m a -> m b -> m (List a)
```

Does the same as `sepBy`, but requires the separating parser to appear at
the end.



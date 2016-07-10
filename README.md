# purescript-simple-parser [![Build Status](https://travis-ci.org/Thimoteus/purescript-simple-parser.svg?branch=master)](https://travis-ci.org/Thimoteus/purescript-simple-parser)

A parsing combinator library with a simple definition.

## Features

Error messages:

```purescript
> parse digit "This is not a digit."
Left ("Expected a digit but found 'T' when trying to parse the string \"This \"...")
```

Backtracking by default:

```purescript
> parse (lower <|> upper <|> digit) "9"
Right ('9')
```

With a `Semigroup` instance (plus appropriate combinators) that does not:

```purescript
parse (string' "hello" <> string' "hell0") "hell0"
Left ("Expecting \"hell0\" but found \"\"")
```

Brand-new combinators (such as `atLeast`, `atMost`, `exactly`, `suchThat`):

```purescript
> parse (fromCharList <$> atLeast 3 (item `suchThat` (_ < 'c'))) "abcd"
Left ("Predicate failed on `suchThat` when trying to parse the string \"cd\"...")
```

Specialized versions of many typeclass functions, to avoid passing class dictionaries:

| Typeclass function | Typeclassless version |
| ------------------ | --------------------- |
|      `<$>`         |        `|->`          |
|      `pure`        |       `pureP`         |
|      `<*>`         |         `~`           |
|       `*>`         |        `>>`           |
|      `<*`          |        `<<`           |
|     `bind`         |        `bindP`        |
|      `>>=`         |        `>>-`          |
|      `=<<`         |        `-<<`          |
|      `>=>`         |        `>->`          |
|      `<=<`         |        `<-<`          |
|      `<|>`         |        `<|`           |
|    `flip alt`      |         `|>`          |

Versions of common combinators optimized for speed,
including a fast stack-safe implementation of `many`:

```purescript
> length longstring
41405
> parse (fromCharList <$> many item) longstring
Right ('asjdf89...
> import Text.Parsing.StringParser as SP
> SP.runParser (fromCharList <$> SP.many SP.anyChar) longstring
RangeError: Maximum call stack size exceeded
```

## Usage

The module `Text.Parsing.Combinators` includes general combinators for use with
any parser that has instances for `Alt`, `MonadPlus`, etc.

The `Text.Parsing.Simple` module has combinators made specifically for its
`Parser` data type.

Combinators that don't backtrack by default are denoted by a prime, for example: `alphanum` vs. `alphanum'`. These are meant to be used in combination with the `Semigroup` instance instead of `Alt`.

Example:

```purescript
import Text.Parsing.Simple
import Text.Parsing.Combinators as C

data PositiveTupleInt = PositiveTupleInt Int Int

parseTupleIntA :: Parser String PositiveTupleInt
parseTupleIntA = PositiveTupleInt <$> (char '(' *> int |= (_ > 0) <* char ',') <*> (int |= (_ > 0) <* char ')')

aprseTupleIntM :: Parser String PositiveTupleInt
parseTupleIntM = fail "Expected TupleInt of the form (x,y)" |> do
  char '('
  fst <- int `suchThat` (_ > 0)
  char ','
  snd <- int `suchThat` (_ > 0)
  char ')'
  return $ PositiveTupleInt fst snd

fromString :: String -> PositiveTupleInt
fromString = either (const $ PositiveTupleInt 1 1) id <<< parse parseTupleIntA
```

Also see the [test](test/) folder.

## Installing

    bower i purescript-simple-parser

## Documentation
- [On Github](docs/Text/Parsing/)
- [On Pursuit](https://pursuit.purescript.org/packages/purescript-simple-parser/)

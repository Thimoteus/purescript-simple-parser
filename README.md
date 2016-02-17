# purescript-simple-parser [![Build Status](https://travis-ci.org/Thimoteus/purescript-simple-parser.svg?branch=master)](https://travis-ci.org/Thimoteus/purescript-simple-parser)

A parsing combinator library with a simple definition. You may prefer
[purescript-string-parsers](https://github.com/paf31/purescript-string-parsers)
if you make heavy use of backtracking, or
[purescript-parsing](https://github.com/purescript-contrib/purescript-parsing)
if your input isn't necessarily a string.

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

Brand-new combinators (such as `atLeast`, `atMost`, `exactly`, `suchThat`):

```purescript
> parse (fromCharList <$> atLeast 3 (item `suchThat` (< 'c'))) "abcd"
Left ("Predicate failed on `suchThat` when trying to parse the string \"cd\"...")
```

Explicit left- and right-leaning versions of Control.Alt.alt (aka (<|>)):

```purescript
vowels :: Parser Char
vowels = fail "Expected a, e, i, o, or u" >|> do
  c <- item
  pure c |= (`elem` ['a', 'e', 'i', 'o', 'u'])
```

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

Example:

```purescript
import Text.Parsing.Simple
import Text.Parsing.Combinators as C

data PositiveTupleInt = PositiveTupleInt Int Int

parseTupleIntA :: Parser PositiveTupleInt
parseTupleIntA = TupleInt <$> (char '(' *> int |= (> 0) <* char ',') <*> (int |= (> 0) <* char ')')

parseTupleIntM :: Parser TupleInt
parseTupleIntM = fail "Expected TupleInt of the form (x,y)" >|> do
  char '('
  fst <- int `suchThat` (> 0)
  char ','
  snd <- int `suchThat` (> 0)
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

# purescript-simple-parser [![Build Status](https://travis-ci.org/Thimoteus/purescript-simple-parser.svg?branch=master)](https://travis-ci.org/Thimoteus/purescript-simple-parser)

A simple parsing library.

## Usage

```purescript
import Text.Parsing.Simple
import Text.Parsing.Combinators as C

data TupleInt = TupleInt Int Int

parseTupleIntA :: Parser TupleInt
parseTupleIntA = TupleInt <$> (char '(' *> int <* char ',') <*> (int <* char ')')

parseTupleIntM :: Parser TupleInt
parseTupleIntM = do
  char '('
  fst <- int
  char ','
  snd <- int
  char ')'
  return $ TupleInt fst snd

fromString :: String -> TupleInt
fromString = fromMaybe (TupleInt 0 0) <<< parse parseTupleIntA
```

Also see the [test](test/) folder.

## Installing

    bower i purescript-simple-parser

## Documentation
- [On Github](docs/Text/Parsing/)
- [On Pursuit](https://pursuit.purescript.org/packages/purescript-simple-parser/)

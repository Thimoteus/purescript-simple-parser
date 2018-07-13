# purescript-simple-parser [![Build Status](https://travis-ci.org/Thimoteus/purescript-simple-parser.svg?branch=master)](https://travis-ci.org/Thimoteus/purescript-simple-parser)

A parsing combinator library with a simple definition.

This is a [transformerless](https://github.com/Thimoteus/purescript-transformerless/) version of [purescript-parsing](https://github.com/purescript-contrib/purescript-parsing/).

Many of the combinators and parsers in the more general parsing library have equivalent versions here, though some are missing.

Furthermore, some are renamed (`Text.Parsing.Simple.bracket` corresponds to `Text.Parsing.Parser.Combinators.between`), and some are completely new.
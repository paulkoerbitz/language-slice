# A Parser and AST for Slice (Specification Language for ZeroC Ice)

[![Build Status](https://travis-ci.org/paulkoerbitz/language-slice.png)](https://travis-ci.org/paulkoerbitz/language-slice)

This module contains an AST and a parser for parsing slice files,
which is the interface definition language used by the RPC library
[ZeroC Ice](http://www.zeroc.com/ice.html).

## Todo

- Improve testsuite
  - More test files
  - Carefully cover all possible cases
- Better error messages
- Fail if a class extends various interfaces
- Accept optional annotation (ice 3.5)
- Accept annotations [["cpp:include:list"]]
- Refactor Parser code to use Attoparsec's built-in methods instead of hand-rolled ones
- Improve 'identifier' parser to correctly account for
  - Scoping token '::'
  - Identifier can't start with 0-9 or _
  - Identifier can't be a keyword
- Write slice generator to generate slice code from AST
- Write name mangler to de-sensify identifiers (to make adding test files easier)

## Done

- Accept default values for structs
- Accept idempotent annotation

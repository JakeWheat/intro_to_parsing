# Intro to Parsing with Parsec in Haskell

WIP, a tutorial which demonstrates the basics of Parsec, builds a
limited SQL query parser, before continuing to build a full featured
SQL:2003 query parser.

# Getting Started

- [`GettingStarted.lhs`](./GettingStarted.lhs)


Introduction to parsing with Parsec, including a review of
Text.Parsec.Char functions.

# Very simple expressions

- [`VerySimpleExpressions.lhs`](./VerySimpleExpressions.lhs)

Creating a very simple expression language parser, and introducing
some functions from Text.Parsec.Combinator.

# Applicative style

- [`ApplicativeStyle.lhs`](./ApplicativeStyle.lhs)

Rewriting the simple expression parser code in a more succinct style.

# Combinator review

- [`CombinatorReview.lhs`](./CombinatorReview.lhs)

Review and examples of all functions from Text.Parsec.Combinator, and
some from Control.Applicative and Control.Monad.

# Functions and types for parsing

- [`FunctionsAndTypesForParsing.lhs`](./FunctionsAndTypesForParsing.lhs)

The utility functions used in the previous tutorials, plus some notes
on types in Parsec.

# Expression parsers

- [`TextParsecExpr.lhs`](./TextParsecExpr.lhs)

This covers using the Text.Parsec.Expr for expression parsing with
prefix, postfix and infix operators with fixity.

# An issue with token parsers

- [`AnIssueWithTokenParsers.lhs`](./AnIssueWithTokenParsers.lhs)

Looks at an issue we have with the way the symbol parser in the
Text.Parsec.Expr tutorial was used, and some possible fixes.

# Permutation parsers

- [`TextParsecPerm.lhs`](./TextParsecPerm.lhs)

This covers the Text.Parsec.Perm module which is used for parsing
different things in flexible order.

# Token parsers

- [`TextParsecToken.lhs`](./TextParsecToken.lhs)

This covers Text.Parsec.Token which can be used to create token
parsers easily.

# Value expressions

- [`ValueExpressions.lhs`](./ValueExpressions.lhs)

This covers building a parser a subset of value expressions from SQL,
which are an extension of the simple expression types and parsers
covered in previous tutorials.

# Query expressions

- [`QueryExpressions.lhs`](./QueryExpressions.lhs)

This covers building a parser to parse query expressions with select
lists, simple from, where, group by, having and order by.

# From clause

- [`FromClause.lhs`](./FromClause.lhs)

This extend the parser for query expressions to support a from clause
with much more features including joins.

# Simple SQL query parser

- [`SimpleSQLQueryParser0.lhs`](./SimpleSQLQueryParser0.lhs)

Here is the code from ValueExpressions, QueryExpressions and
FromClause plus tests put together and rearranged as a coherent
standalone module.

# Pretty printing

- [`PrettyPrinting0.lhs`](./PrettyPrinting0.lhs)

This quick module covers a simple pretty printer for our SQL ast.

# Error messages

- [`ErrorMessages.lhs`](./ErrorMessages.lhs)

In this document, we will explore error messages with parsec and how
restructuring parser code can lead to better or worse error messages.

# Later documents

Additional documents not yet started:

## Parsing TPC-H queries

We will use the tpch queries as examples to help improve the pretty
printer. First there are a few extra bits of syntax to be able to
parse these queries

## Pretty printing part 2

some tweaks to the pretty printer to improve the layout for the tpch
queries

## Writing tests

Here we will take the ad hoc tests and build an organised test suite
with a wrapper for hunit, wrapper for test.framework wrapper and maybe
tasty

## Refactored project + cabal package

In this tutorial, we will take the sql parser, pretty printer and
tests, and create a complete cabal package.

TODO: talk about robustness and the casual way the parser has been put
together and the casual way issues have been tackled.

# The following might go in a separate project/ tutorial

## Writing a command line sql interface

quick experiment to try to implement the front end for a multiline sql
command line using fake incremental parsing which parsec doesn't
support directly.

## Parsec internals

In this tutorial, we will try to understand a bit more about the
implementation of Parsec. We will start by implementing a simple
parser combinator library from scratch.

## ANSI SQL grammar

In this really interesting document, we will go through the entire
SQL:2003 grammar, skipping the bits which aren't relevant to queries,
and try to understand and give examples of all the different
bits. This will be used as a guide to complete the parser so that it
can parse all SQL:2003 queries.

## Extending value expressions to cover more features

In this tutorial(s), we will cover adding all the missing bits in
value expressions from the ANSI SQL:2003 standard.
some ideas:
left factor expression parser
number literals
typed literal
subqueries
full dotted
quoted identifiers/ names
more operators + trickiness with between
splitting the operator ast ctor
aggregate calls
window functions
other value expression things

## Extending query expressions to cover more features

In this tutorial(s), we will extend the parser to cover all the other
missing features from SQL:2003.
some ideas:
select distinct and select all
union, except, intersect
offset, fetch
explicit table, table value constructor
cte
full table alias
* as (a,b,c)
dotted identifiers in table names, etc.
lateral
tableref function
group by extensions
order by asc, desc, nulls first/last

## Position annotation

In this tutorial, we will add position annotation to the parsing, so
that a later stage could, e.g., provide type error messages with the
correct line and column numbers.

## Dialects

In this tutorial, we will discuss how we can support other SQL dialects

## Separate lexer

In this tutorial, we will look at creating a proper separate lexer to
see how it is done, and remark on what the tradeoffs seem to be.

## Quasiquotes

In this tutorial, we will create quasiquoters for sql query
expressions and value expressions, and see how powerful this can be

## Speed and optimisation

maybe some benchmarking: parsing from different sources (memory, disk,
string, strict text, lazy text), compare lex no lex + attoparsec. Look
at the lib for attoparsec and parsec together (one for speed and the
other to retry to get good error messages).

## Something about syntax highlighting, generating documentation + links


# Extras

- [`ParseString.lhs`](./ParseString.lhs)

an executable which contains the boilerplate to run a parsec parser on
a string passed as an argument

- [`ParseFile.lhs`](./ParseFile.lhs)

an executable which contains the boilerplate to run a parsec parser on
a file passed as an argument

# TODO: add the parsec wrapper modules with simplified types

Contact: jakewheatmail@gmail.com

License: BSD3

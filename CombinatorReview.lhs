
= Overview

In this tutorial we will go through all the functions in
Text.Parsec.Combinator, and some useful ones in Control.Applicative
and Control.Monad as well.

> import Text.Parsec (ParseError)
> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Parsec (try)
> import Text.Parsec.String.Char (oneOf, char, digit
>                                ,string, letter, satisfy)
> import Text.Parsec.String.Combinator (many1, choice, chainl1, between
>                                      ,count, option, optionMaybe, optional)
> import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), (<|>), many)
> import Control.Monad (void, ap, mzero)
> import Data.Char (isLetter, isDigit)
> import FunctionsAndTypesForParsing

= Text.Parsec.Combinator

You should look at the source for these functions and try to
understand how they are implemented.

<http://hackage.haskell.org/package/parsec-3.1.3/docs/src/Text-Parsec-Combinator.html>

The style of the source code in the Parsec library sources is a little
different to what we used at the end of the last tutorial. You can try
reimplementing each of the Text.Parsec.Combinator module functions
using the Applicative style. See if you can find a way to reassure
yourself that the rewritten versions you make are correct, perhaps via
writing automated tests, or perhaps some other method.

You should be able to easily understand the implementation of all the
functions in Text.Parsec.Combinator except possibly `anyToken` and
`notFollowedBy`.

== choice

```haskell
choice :: [Parser a] -> Parser a
```

`choice ps` tries to apply the parsers in the list `ps` in order,
until one of them succeeds. It returns the value of the succeeding
parser.

> a :: Parser Char
> a = char 'a'
>
> b :: Parser Char
> b = char 'b'
>
> aOrB :: Parser Char
> aOrB = choice [a,b]

```
*Main> regularParse aOrB "a"
Right 'a'

*Main> regularParse aOrB "b"
Right 'b'

*Main> regularParse aOrB "c"
Left (line 1, column 1):
unexpected "c"
expecting "a" or "b"
```

=== using with try

If a parser fails with `(<|>)` or `choice`, then it will only try the
next parser if the last parser consumed no input.

TODO: make the parsers return the keyword and update the examples

> byKeyword :: Parser ()
> byKeyword = void $ string "by"
>
> betweenKeyword :: Parser ()
> betweenKeyword = void $ string "between"

Since both of these have the same prefix - b - if we combine them
using `choice` then it doesn't work correctly:

```
*Main> regularParse byKeyword "by"
Right ()

*Main> regularParse byKeyword "between"
Left (line 1, column 1):
unexpected "e"
expecting "by"

*Main> regularParse betweenKeyword "between"
Right ()

*Main> regularParse betweenKeyword "by"
Left (line 1, column 1):
unexpected "y"
expecting "between"

*Main> regularParse (choice [betweenKeyword,byKeyword]) "between"
Right ()

*Main> regularParse (choice [betweenKeyword,byKeyword]) "by"
Left (line 1, column 1):
unexpected "y"
expecting "between"

*Main> regularParse (choice [byKeyword,betweenKeyword]) "between"
Left (line 1, column 1):
unexpected "e"
expecting "by"

*Main> regularParse (choice [byKeyword,betweenKeyword]) "by"
Right ()
```

If we use `try` on the first option, then it all works fine.

```
*Main> regularParse (choice [try byKeyword,betweenKeyword]) "by"
Right ()

*Main> regularParse (choice [try byKeyword,betweenKeyword]) "between"
Right ()
```

== count

```haskell
count :: Int -> Parser a -> Parser [a]
```

`count n p` parses `n` occurrences of `p`. If `n` is smaller or equal
to zero, the parser is equivalent to `return []`. It returns a list of
the n values returned by `p`.

```
*Main> regularParse (count 5 a) "aaaaa"
Right "aaaaa"

*Main> regularParse (count 5 a) "aaaa"
Left (line 1, column 5):
unexpected end of input
expecting "a"

*Main> regularParse (count 5 a) "aaaab"
Left (line 1, column 5):
unexpected "b"
expecting "a"

*Main> regularParse (count 5 aOrB) "aabaa"
Right "aabaa"
```

== between

```haskell
between :: Parser open -> Parser close -> Parser a -> Parser a
```

`between open close p` parses `open`, followed by `p` and
`close`. It returns the value returned by `p`.

We can replace the betweenParens from the previous tutorial using
this:

> betweenParens :: Parser a -> Parser a
> betweenParens p = between (symbol '(') (symbol ')') p

It hardly seems worth it to make this change, but it might be slightly
quicker to read and understand if you aren't already familiar with
some code or haven't viewed it for a while. This is good for 'code
maintenance', where we need to fix bugs or add new features quickly to
code we haven't looked at for two years or something.

Here are the support functions for this parser.

> symbol :: Char -> Parser Char
> symbol c = lexeme $ char c

> lexeme :: Parser a -> Parser a
> lexeme p = p <* whitespace

> whitespace :: Parser ()
> whitespace = void $ oneOf " \n\t"

== option

```haskell
option :: a -> Parser a -> Parser a
```

`option x p` tries to apply parser `p`. If `p` fails without
consuming input, it returns the value `x`, otherwise the value returned
by `p`.

```
*Main> regularParse (option "" (count 5 aOrB)) "aaaaa"
Right "aaaaa"

*Main> regularParse (option "" (count 5 aOrB)) "caaaa"
Right ""

*Main> regularParse (option "" (count 5 aOrB)) "aaaa"
Left (line 1, column 5):
unexpected end of input
expecting "a" or "b"

*Main> regularParse (option "" (count 5 aOrB)) "aaaac"
Left (line 1, column 5):
unexpected "c"
expecting "a" or "b"

*Main> regularParse (option "" (try (count 5 aOrB))) "aaaa"
Right ""
```

== optionMaybe

```haskell
optionMaybe :: Parser a -> Parser (Maybe a)
```

`optionMaybe p` tries to apply parser `p`. If `p` fails without consuming
input, it returns `Nothing`, otherwise it returns `Just` the value returned
by `p`.

```
*Main> regularParse (optionMaybe (count 5 aOrB)) "aaaaa"
Right (Just "aaaaa")

*Main> regularParse (optionMaybe (count 5 aOrB)) "caaaa"
Right Nothing

*Main> regularParse (optionMaybe (count 5 aOrB)) "caaa"
Right Nothing

*Main> regularParse (optionMaybe (count 5 aOrB)) "aaaa"
Left (line 1, column 5):
unexpected end of input
expecting "a" or "b"

*Main> regularParse (optionMaybe (count 5 aOrB)) "aaaac"
Left (line 1, column 5):
unexpected "c"
expecting "a" or "b"

*Main> regularParse (optionMaybe (try $ count 5 aOrB)) "aaaac"
Right Nothing
```

== optional

```haskell
optional :: Parser a -> Parser ()
```

`optional p` tries to apply parser `p`. It will parse `p` or
nothing. It only fails if `p` fails after consuming input. It discards
the result of `p`.

```
*Main> parseWithLeftOver (optional (count 5 aOrB)) "aaaaa"
Right ((),"")

*Main> parseWithLeftOver (optional (count 5 aOrB)) "caaaa"
Right ((),"caaaa")

*Main> parseWithLeftOver (optional (count 5 aOrB)) "caaa"
Right ((),"caaa")

*Main> parseWithLeftOver (optional (count 5 aOrB)) "aaaa"
Left (line 1, column 5):
unexpected end of input
expecting "a" or "b"

*Main> parseWithLeftOver (optional (count 5 aOrB)) "aaaac"
Left (line 1, column 5):
unexpected "c"
expecting "a" or "b"

*Main> parseWithLeftOver (optional (try $ count 5 aOrB)) "aaaac"
Right ((),"aaaac")
```

== skipMany1

```haskell
skipMany1 :: Parser a -> Parser ()
```

`skipMany1 p` applies the parser `p` one or more times, skipping its result.

== many1

```haskell
many1 :: Parser a -> Parser [a]
```

many1 p applies the parser p one or more times. Returns a list of the
returned values of p.

```haskell
  word  = many1 letter
```

== sepBy

```haskell
sepBy :: Parser a -> Parser sep -> Parser [a]
```

sepBy p sep parses zero or more occurrences of p, separated by
sep. Returns a list of values returned by p.

```haskell
  commaSep p  = p `sepBy` (symbol ",")
```

== sepBy1

```haskell
sepBy1 :: Parser a -> Parser sep -> Parser [a]
```

sepBy1 p sep parses one or more occurrences of p, separated by
sep. Returns a list of values returned by p.

== endBy

```haskell
endBy :: Parser a -> Parser sep -> Parser [a]
```

endBy p sep parses zero or more occurrences of p, seperated and ended
by sep. Returns a list of values returned by p.

```haskell
   cStatements  = cStatement `endBy` semi
```

== endBy1

```haskell
endBy1 :: Parser a -> Parser sep -> Parser [a]
```

endBy1 p sep parses one or more occurrences of p, seperated and ended
by sep. Returns a list of values returned by p.

== sepEndBy

```haskell
sepEndBy :: Parser a -> Parser sep -> Parser [a]
```

sepEndBy p sep parses zero or more occurrences of p, separated and
optionally ended by sep, ie. haskell style statements. Returns a list
of values returned by p.

```haskell
  haskellStatements  = haskellStatement `sepEndBy` semi
```

== sepEndBy1

```haskell
sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
```

sepEndBy1 p sep parses one or more occurrences of p, separated and
optionally ended by sep. Returns a list of values returned by p.

== chainl

```haskell
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
```

chainl p op x parser zero or more occurrences of p, separated by
op. Returns a value obtained by a left associative application of all
functions returned by op to the values returned by p. If there are
zero occurrences of p, the value x is returned.

== chainl1

```haskell
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
```

chainl1 p op x parser one or more occurrences of p, separated by op
Returns a value obtained by a left associative application of all
functions returned by op to the values returned by p. . This parser
can for example be used to eliminate left recursion which typically
occurs in expression grammars.

== chainr

```haskell
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
```

chainr p op x parser zero or more occurrences of p, separated by op
Returns a value obtained by a right associative application of all
functions returned by op to the values returned by p. If there are no
occurrences of p, the value x is returned.

== chainr1

```haskell
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
```

chainr1 p op x parser one or more occurrences of `p`, separated by op
Returns a value obtained by a right associative application of all
functions returned by op to the values returned by p.

== eof

```haskell
eof :: Parser ()
```

This parser only succeeds at the end of the input. This is not a
primitive parser but it is defined using notFollowedBy.

```haskell
  eof  = notFollowedBy anyToken <?> "end of input"
```

The (<?>) operator is used for error messages. We will come back to
error messages after writing the basic SQL parser.

== notFollowedBy

```haskell
notFollowedBy :: Show a => Parser a -> Parser ()
```

notFollowedBy p only succeeds when parser p fails. This parser does
not consume any input. This parser can be used to implement the
'longest match' rule. For example, when recognizing keywords (for
example let), we want to make sure that a keyword is not followed by a
legal identifier character, in which case the keyword is actually an
identifier (for example lets). We can program this behaviour as
follows:

```haskell
  keywordLet  = try (do{ string "let"
                       ; notFollowedBy alphaNum
                       })
```

== manyTill

```haskell
manyTill :: Parser a -> Parser end -> Parser [a]
```

manyTill p end applies parser p zero or more times until parser end
succeeds. Returns the list of values returned by p. This parser can be
used to scan comments:

```haskell
  simpleComment   = do{ string "<!--"
                      ; manyTill anyChar (try (string "-->"))
                      }
```
// asciidoc: the string should be "-->"

Note the overlapping parsers anyChar and string "-\->", and therefore
the use of the try combinator.

== lookAhead

```haskell
lookAhead :: Parser a -> Parser a
```

lookAhead p parses p without consuming any input.

If p fails and consumes some input, so does lookAhead. Combine with
try if this is undesirable.

== anyToken

```haskell
anyToken :: Parser Char
```

The parser anyToken accepts any kind of token. It is for example used
to implement eof. Returns the accepted token.

= Control.Applicative

Here are the functions from Applicative that are used:

`(<$>)`, `(<*>)`, `(<$)`, `(<*)`, `(*>)`, `(<|>)`, `many`

TODO: examples for all of these

We've already seen all of these, except `(<$)`. This is often used to
parse a keyword and return a no argument constructor:

> data Something = Type1 | Type2 | Type3

> something :: Parser Something
> something = choice [Type1 <$ string "type1"
>                    ,Type2 <$ string "type2"
>                    ,Type3 <$ string "type3"]

// the first symbol following should be (<**>)
// the backslashes are for asciidoc
// todo: do this in the render.lhs?
// I wish I understood why you need backslashes in some places
// and not others

There is also `(<\**>)` which is `(<*>)` with the arguments flipped.

TODO: double check using these from Parsec instead of
Control.Applicative: possible performance implictions? Look at the
implementations, maybe they are all the same now, and the performance
issues were in the past only.

= Control.Monad

== return

One use of return is to always succeed, and return a value:

> alwaysX :: Parser Char
> alwaysX = return 'x'

```
*Main> parseWithLeftOver (a <|> alwaysX) "a"
Right ('a',"")

*Main> parseWithLeftOver (a <|> alwaysX) "b"
Right ('x',"b")
```

== mzero

This function is used in the implementation of `choice`:

> choice' :: [Parser a] -> Parser a
> choice' ps = foldr (<|>) mzero ps



TODO: go through a bunch of functions + do notation examples

 >>=
 =<<
 >>
 void
 mapM, mapM_
 sequence,sequence_
 guard
 return
mzero
mplus
when, unless
liftMN
ap
quick note about fail, will return to this in the error messages stage

TODO: using trace

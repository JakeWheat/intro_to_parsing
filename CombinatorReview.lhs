
In this tutorial we will go through all the functions in
Text.Parsec.Combinator, and some useful on es in Control.Applicative
and Control.Monad as well.

> import Text.Parsec (ParseError)
> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Parsec (try)
> import Text.Parsec.String.Char (oneOf, char, digit
>                                ,string, letter, satisfy)
> import Text.Parsec.String.Combinator (many1, choice, chainl1, between)
> import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), (<|>), many)
> import Control.Monad (void, ap)
> import Data.Char (isLetter, isDigit)
> import Boilerplate
> import ApplicativeStyle

= Text.Parsec.Combinator

Now we will go through the Text.Parsec.Combinator module. You should
look at the source for these functions and try to understand how they
are implemented. The style of the source code is a little different -
you can also try reimplementing each of them using the Applicative
style.

I've included the Haddock documentation for each function, and some
examples of each.

```
choice :: [Parser a] -> Parser a
```

Haddock: choice ps tries to apply the parsers in the list ps in order, until
one of them succeeds. Returns the value of the succeeding parser.

We've already seen this one in use.

```
count :: Int -> Parser a -> Parser [a]
```

Haddock: count n p parses n occurrences of p. If n is smaller or equal
to zero, the parser equals to return []. Returns a list of n values
returned by p.

TODO: examples

```
between :: Parser open -> Parser close -> Parser a -> Parser a
```

Haddock: between open close p parses open, followed by p and close. Returns the
value returned by p.

```
  braces  = between (symbol "{") (symbol "}")
```

We can replace the betweenParens using this:

> betweenParens' :: Parser a -> Parser a
> betweenParens' p = between (symbol '(') (symbol ')') p

It hardly seems worth it, but if you are already familiar with
between, then it is quicker to read, which helps in a situation like
when you are in a rush and coming back to code you wrote 3 years ago,
for instance.


```
option :: a -> Parser a -> Parser a
```

Haddock: option x p tries to apply parser p. If p fails without
consuming input, it returns the value x, otherwise the value returned
by p.

```
  priority  = option 0 (do{ d <- digit
                          ; return (digitToInt d)
                          })
```

```
optionMaybe :: Parser a -> Parser (Maybe a)
```

optionMaybe p tries to apply parser p. If p fails without consuming
input, it returns Nothing, otherwise it returns Just the value returned
by p.

```
optional :: Parser a -> Parser ()
```

optional p tries to apply parser p. It will parse p or nothing. It
only fails if p fails after consuming input. It discards the result of
p.

```
skipMany1 :: Parser a -> Parser ()
```

skipMany1 p applies the parser p one or more times, skipping its result.

```
many1 :: Parser a -> Parser [a]
```

many1 p applies the parser p one or more times. Returns a list of the
returned values of p.

```
  word  = many1 letter
```

```
sepBy :: Parser a -> Parser sep -> Parser [a]
```

sepBy p sep parses zero or more occurrences of p, separated by
sep. Returns a list of values returned by p.

```
  commaSep p  = p `sepBy` (symbol ",")
```

```
sepBy1 :: Parser a -> Parser sep -> Parser [a]
```

sepBy1 p sep parses one or more occurrences of p, separated by
sep. Returns a list of values returned by p.

```
endBy :: Parser a -> Parser sep -> Parser [a]
```

endBy p sep parses zero or more occurrences of p, seperated and ended
by sep. Returns a list of values returned by p.

```
   cStatements  = cStatement `endBy` semi
```

```
endBy1 :: Parser a -> Parser sep -> Parser [a]
```

endBy1 p sep parses one or more occurrences of p, seperated and ended
by sep. Returns a list of values returned by p.

```
sepEndBy :: Parser a -> Parser sep -> Parser [a]
```

sepEndBy p sep parses zero or more occurrences of p, separated and
optionally ended by sep, ie. haskell style statements. Returns a list
of values returned by p.

```
  haskellStatements  = haskellStatement `sepEndBy` semi
```

```
sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
```

sepEndBy1 p sep parses one or more occurrences of p, separated and
optionally ended by sep. Returns a list of values returned by p.

```
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
```

chainl p op x parser zero or more occurrences of p, separated by
op. Returns a value obtained by a left associative application of all
functions returned by op to the values returned by p. If there are
zero occurrences of p, the value x is returned.

```
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
```

chainl1 p op x parser one or more occurrences of p, separated by op
Returns a value obtained by a left associative application of all
functions returned by op to the values returned by p. . This parser
can for example be used to eliminate left recursion which typically
occurs in expression grammars.

TODO: reimplement the Add suffix parser using chainl1

```
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
```

chainr p op x parser zero or more occurrences of p, separated by op
Returns a value obtained by a right associative application of all
functions returned by op to the values returned by p. If there are no
occurrences of p, the value x is returned.

```
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
```

chainr1 p op x parser one or more occurrences of |p|, separated by op
Returns a value obtained by a right associative application of all
functions returned by op to the values returned by p.

```
eof :: Parser ()
```

This parser only succeeds at the end of the input. This is not a
primitive parser but it is defined using notFollowedBy.

```
  eof  = notFollowedBy anyToken <?> "end of input"
```

The (<?>) operator is used for error messages. We will come back to
error messages after writing the basic SQL parser.

```
notFollowedBy :: Show a => Parser a -> Parser ()
```

notFollowedBy p only succeeds when parser p fails. This parser does
not consume any input. This parser can be used to implement the
'longest match' rule. For example, when recognizing keywords (for
example let), we want to make sure that a keyword is not followed by a
legal identifier character, in which case the keyword is actually an
identifier (for example lets). We can program this behaviour as
follows:

```
  keywordLet  = try (do{ string "let"
                       ; notFollowedBy alphaNum
                       })
```

```
manyTill :: Parser a -> Parser end -> Parser [a]
```

manyTill p end applies parser p zero or more times until parser end
succeeds. Returns the list of values returned by p. This parser can be
used to scan comments:

```
  simpleComment   = do{ string "<!--"
                      ; manyTill anyChar (try (string "-->"))
                      }
```

Note the overlapping parsers anyChar and string "-->", and therefore
the use of the try combinator.

```
lookAhead :: Parser a -> Parser a
```

lookAhead p parses p without consuming any input.

If p fails and consumes some input, so does lookAhead. Combine with
try if this is undesirable.

```
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

There is also `(<**>)` which is `(<*>)` with the arguments flipped.

TODO: double check using these from Parsec instead of
Control.Applicative: possible performance implications? Look at the
implementations, maybe they are all the same now, and the performance
issues were in the past only.

= Control.Monad

TODO: go through a bunch of functions + do notation examples

 >>=
 =<<
 >>
 void
 mapM, mapM_
 sequence,sequence_
 guard
mzero
mplus
when, unless
liftMN
ap
quick note about fail, will return to this in the error messages stage

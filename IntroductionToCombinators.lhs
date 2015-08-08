
= Introduction to combinators

In this tutorial we will develop a parser for a very simple expression
language, and start learning about the set of combinators which comes
with Parsec.

> import Text.Parsec (ParseError)
> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Parsec (try)
> import Text.Parsec.String.Char (oneOf, char, digit
>                                ,string, letter, satisfy)
> import Text.Parsec.String.Combinator (many1, choice, chainl1, between)
> import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), (<|>), many)
> import Control.Monad (void, ap)
> import Data.Char (isLetter, isDigit)
> import FunctionsAndTypesForParsing

Let's create a very simple expression language:

> data SimpleExpr = Num Integer
>                 | Var String
>                 | Add SimpleExpr SimpleExpr
>                 | Parens SimpleExpr
>                   deriving (Eq,Show)

It's a bit simple and almost useless at the moment, but we will expand
on this a lot in later tutorials.

Here are some examples:

> simpleExprExamples :: [(String,SimpleExpr)]
> simpleExprExamples =
>     [("a", Var "a")
>     ,("1", Num 1)
>     ,("2 + 3", Add (Num 2) (Num 3))
>     ,("(42)", Parens (Num 42))]

TODO: some more complex examples

Let's write a simple parser for these, and introduce a few things
along the way.

We will write a parser for each constructor separately, then look at
how we can write a parser for all of them together.

==== Num

To parse a number, we need to parse one or more digits, and then read
the resulting string. We can use the combinator 'many1' to help with
this. We will also use do notation.

> num :: Parser SimpleExpr
> num = do
>     n <- many1 digit
>     return (Num (read n))

Let's try it out. TODO: examples

How does it work? First, we parse one or more (many1) digits (digit),
and give the result the name 'n'. Then we convert the string to an
integer using read, and wrap it in a Num constructor.

The many1 function's type looks like this:

```
many1 :: Parser a -> Parser [a]
```

It applies the parser given one or more times, returning the result.

TODO: example show what happens when you use 'many' instead of 'many1'

==== Var

For var, we have to decide on a syntax for the identifiers. Let's go
for a common choice: identifiers must start with a letter or
underscore, and then they can be followed by zero or more letters,
underscores or digits in any combination.

> var :: Parser SimpleExpr
> var = do
>     fc <- firstChar
>     rest <- many nonFirstChar
>     return (Var (fc:rest))
>   where
>     firstChar = satisfy (\a -> isLetter a || a == '_')
>     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

This time, we create two helper parsers: firstChar - which parses a
letter or underscore, and nonFirstChar = which parses a digit, letter
or underscore. This time, we use the 'many' function instead of
'many1': TODO - demonstrate why using examples only.

> add :: Parser SimpleExpr
> add = do
>     e0 <- num
>     void $ char '+'
>     e1 <- num
>     return (Add e0 e1)

There are two simplifications here. We don't have a general simple
expression parser yet, so it only supports parsing adding two number
literals, and we don't deal with whitespace yet, so you must write
"1+2" and not "1 + 2". We will deal with both of these issues below.

We used void to ignore the return of "char '+'". This is not required,
but supresses a warning which you should get (since you are using
-Wall, right?) and I think it is also good style to explicitly say
that the result is being ignored.

To use -Wall in ghci, enter the following at the prompt:

```
*Main> :set -Wall
```

Try it out, then replace the line

```
    void $ char '+'
```

with

```
    char '+'
```

And check you see the warning. Another way of avoiding the warning is
to write this:

```
    _ <- char '+'
```

==== parens

> parens :: Parser SimpleExpr
> parens = do
>     void $ char '('
>     e <- num
>     void $ char ')'
>     return (Parens e)

The same two issues from the 'add' parser apply here: whitespace and
lack of a general simple expression parser, so it just uses the num
parser again instead. Now we will tackle the whitespace issue.

=== whitespace and lexeme parsing

Here is a parser which will skip zero or more whitespace characters.

> whiteSpace :: Parser ()
> whiteSpace = void $ many $ oneOf " \n\t"

In the original parsec documentation, one of the concepts mentioned is
the idea of 'lexeme' parsing. This is a style in which every token
parser should also consume and ignore any trailing whitespace. This is
a simple convention which with a bit of care allows skipping
whitespace exactly once wherever it needs to be skipped. To complete
the lexeme style, we should also always skip leading whitespace at the
top level only.

> parseWithWhitespace :: Parser a -> String -> Either ParseError a
> parseWithWhitespace p = parseWithEof wrapper
>   where
>     wrapper = do
>         whiteSpace
>         p

the wrapper function can also use (>>) to make it a bit shorter:

```
wrapper = whiteSpace >> p
```

Here is a shorter version of this function using (>>):

> parseWithWhitespace' :: Parser a -> String -> Either ParseError a
> parseWithWhitespace' p = parseWithEof (whiteSpace >> p)

Here is the num parser rewritten in the lexeme style:

> lexeme :: Parser a -> Parser a
> lexeme p = do
>            x <- p
>            whiteSpace
>            return x

TODO: review the placement of the function 'lexeme' in all the code
below. Maybe something could be said about all the places that it can
be put.

> num' :: Parser SimpleExpr
> num' = lexeme $ do
>     n <- many1 digit
>     return (Num (read n))

Here it is in action:

```
*Main Data.List> parseWithEof num "1"
Right (Num 1)

*Main Data.List> parseWithEof num " 1"
Left (line 1, column 1):
unexpected " "
expecting digit

*Main Data.List> parseWithEof num "1 "
Left (line 1, column 2):
unexpected ' '
expecting digit or end of input

*Main Data.List> parseWithEof num " 1 "
Left (line 1, column 1):
unexpected " "
expecting digit

*Main Data.List> parseWithWhitespace num' "1"
Right (Num 1)

*Main Data.List> parseWithWhitespace num' " 1"
Right (Num 1)

*Main Data.List> parseWithWhitespace num' "1 "
Right (Num 1)

*Main Data.Lst> parseWithWhitespace num' " 1 "
Right (Num 1)
```

Here are the other functions in lexeme style.

> var' :: Parser SimpleExpr
> var' = lexeme $ do
>     fl <- firstChar
>     rest <- many nonFirstChar
>     return (Var (fl:rest))
>   where
>     firstChar = satisfy (\a -> isLetter a || a == '_')
>     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

> add' :: Parser SimpleExpr
> add' = do
>     e0 <- num'
>     void $ lexeme $ char '+'
>     e1 <- num'
>     return (Add e0 e1)

> parens' :: Parser SimpleExpr
> parens' = do
>     void $ lexeme $ char '('
>     e <- num'
>     void $ lexeme $ char ')'
>     return (Parens e)

In this style, you have to be slightly careful to make sure you call
whitespace at the right points.

Let's try and implement the simpleExpr parser. We can use a function
called 'choice':

> numOrVar :: Parser SimpleExpr
> numOrVar = choice [num', var']

It tries each parser one at a time, finishing with the first one that
succeeds. There are some more details about this later on.

Here is another way to write the numOrVar parser:

> numOrVar' :: Parser SimpleExpr
> numOrVar' = num' <|> var'

In general, you can write 'choice [p0, p1, p2, ...]' as 'p0 <|> p1 <|>
p2 <|> ...'.

TODO: a bunch of examples

Here is the first version of the simpleExpr parser:

> simpleExpr :: Parser SimpleExpr
> simpleExpr = choice [num', var', add', parens']

TODO: a bunch of examples

It works well for some of the parsers, but fails with add': the num'
always partially succeeds first, then fails, so the add' is never
tried. We can rearrange this parser like this:

> simpleExpr' :: Parser SimpleExpr
> simpleExpr' = choice [add', num', var', parens']

TODO: examples again

We have another problem now: when we start parsing the add', see a
num, then fail, it gives up completely. This is because the 'choice'
function (and <|>) will only try the next parser if the parser that
failed consumed no input before failing.

TODO: show examples with satify and choice on 1 char match/no match,
and two char match/no match.

Here is one way to fix it:

> simpleExpr'' :: Parser SimpleExpr
> simpleExpr'' = choice [try add', num', var', parens']

The try function implements backtracking. When this is used in a
choice like this, it means that if the add' parser fails, it will undo
the consumed input and carry on with the next option, instead of
failing completely. If there is another place using try higher up in
the call stack, then we will continue there, otherwise the whole parse
will fail immediately.

The same happens with <|>, we can implement the simpleExpr parser like
this also:

> simpleExpr''' :: Parser SimpleExpr
> simpleExpr''' = try add' <|> num' <|> var' <|> parens'

TODO: show the examples all working

Now we can make 'parens' and 'add' use a general simple expression
parser. Parens is simple:

> parens'' :: Parser SimpleExpr
> parens'' = do
>     void $ lexeme $ char '('
>     e <- simpleExpr'''
>     void $ lexeme $ char ')'
>     return (Parens e)

There is a problem implementing 'add' in the same way:

> add'' :: Parser SimpleExpr
> add'' = do
>     e0 <- simpleExpr'''
>     void $ lexeme $ char '+'
>     e1 <- simpleExpr'''
>     return (Add e0 e1)

It will never return since it calls simpleExpr''' which calls add''
again.

Let's look at another problem:

```
*Main> parseWithWhitespace simpleExpr''' " 1 + 1 + 1"
Left (line 1, column 8):
unexpected '+'
expecting end of input
```

Our parser will only parse one operator, and not a chain of them.

Here is one way to solve it:

> simpleExpr4 :: Parser SimpleExpr
> simpleExpr4 = do
>     e <- term
>     maybeAddSuffix e
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 = do
>         void $ lexeme $ char '+'
>         e1 <- term
>         maybeAddSuffix (Add e0 e1)
>     term = num' <|> var' <|> parens'

TODO: explain how this works in much more detail

This forces our Add operator to be left associative. This also solves
the previous problem with add calling simpleExpr recursively. There is
lots of discussion about these issues in Parsing theory documents you
can find online, etc..

== general simple expression parser

Here is the all the parser code written out again for clarity.

> numD :: Parser SimpleExpr
> numD = lexeme $ do
>     n <- many1 digit
>     return $ Num $ read n

> varD :: Parser SimpleExpr
> varD = lexeme $ do
>     fl <- firstChar
>     rest <- many nonFirstChar
>     return $ Var (fl:rest)
>   where
>     firstChar = satisfy (\a -> isLetter a || a == '_')
>     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

> parensD :: Parser SimpleExpr
> parensD = do
>     void $ lexeme $ char '('
>     e <- simpleExprD
>     void $ lexeme $ char ')'
>     return $ Parens e

> simpleExprD :: Parser SimpleExpr
> simpleExprD = do
>     e <- term
>     maybeAddSuffix e
>   where
>     maybeAddSuffix e =
>         choice [addSuffix e
>                ,return e]
>     addSuffix e0 = do
>         void $ lexeme $ char '+'
>         e1 <- term
>         maybeAddSuffix (Add e0 e1)
>     term = numD <|> varD <|> parensD


=== Testing with the examples

TODO: write a little manual tester that accepts a parser and a list of
examples, and checks they all parse correctly.

=== Testing with quickcheck

Let's see if we can check with quickcheck. It's a bit tricky testing
parsers in this way, but one way to do something useful is to generate
random asts, convert them to concrete syntax, parse them, and check
the result. We can write a simple 'pretty printer' to convert an ast
to concrete syntax.

==== a pretty printer

TODO: a really simple pretty printer just pasting strings together, no
layout.

==== the quick check code

TODO: write a quickcheck property and arbitary instance and show
running it at the ghci prompt


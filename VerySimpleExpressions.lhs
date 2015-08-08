
= Overview

In this tutorial we will develop a parser for a very simple expression
language, and start learning about the set of combinators which comes
with Parsec.

> import Text.Parsec (ParseError)
> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Parsec (try)
> import Text.Parsec.String.Char (oneOf, char, digit, satisfy)
> import Text.Parsec.String.Combinator (many1, choice, chainl1)
> import Control.Applicative ((<|>), many)
> import Control.Monad (void)
> import Data.Char (isLetter, isDigit)
> import FunctionsAndTypesForParsing

= num

The first element we will have in this expression language is positive
integral numbers:

> numberExamples :: [(String,Integer)]
> numberExamples = [("1", 1)
>                  ,("23", 23)]

TODO: make examples with parsing failures for all of the example
scripts below?

To parse a number, we need to parse one or more digits, and then read
the resulting string. We can use the combinator `many1` to help with
this. We will also use do notation.

> num :: Parser Integer
> num = do
>     n <- many1 digit
>     return (read n)

Let's try it out.

```
*Main> regularParse num "1"
Right 1

*Main> regularParse num "123456"
Right 123456

*Main> regularParse num "aa"
Left (line 1, column 1):
unexpected "a"
expecting digit

```

How does it work? First, we parse one or more (`many1`) digits (`digit`),
and give the result the name 'n'. Then we convert the string to an
integer using `read`.

The `many1` function's type looks like this:

```
many1 :: Parser a -> Parser [a]
```

It applies the parser given one or more times, returning the result.

Let's see what happens when we use the `many` combinator which parses
zero or more items instead of one or more.

> num1 :: Parser Integer
> num1 = do
>     n <- many digit
>     return (read n)

```
*Main> regularParse num1 "1"
Right 1

*Main> regularParse num1 "123456"
Right 123456

*Main> regularParse num1 "aa"
Right *** Exception: Prelude.read: no parse
```

= var

For var, we have to decide on a syntax for the identifiers. Let's go
for a common choice: identifiers must start with a letter or
underscore, and then they can be followed by zero or more letters,
underscores or digits in any combination.

> varExamples :: [(String,String)]
> varExamples = [("test", "test")
>               ,("_stuff", "_stuff")
>               ,("_1234", "_1234")]

> var :: Parser String
> var = do
>     fc <- firstChar
>     rest <- many nonFirstChar
>     return (fc:rest)
>   where
>     firstChar = satisfy (\a -> isLetter a || a == '_')
>     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

This time, we create two helper parsers: `firstChar`, which parses a
letter or underscore, and `nonFirstChar` which parses a digit, letter
or underscore. This time, we use the `many` function instead of
`many1`.

Try it out in ghci. I like to try things which you expect to work, and
also to try things which you expect to not work and make sure you get
an error.

= parens

The parens parser will eventually parse any expression inside
parentheses. First it will just parse integers inside parentheses.

> data Parentheses = Parentheses Integer
>                    deriving (Eq,Show)
>
> parensExamples :: [(String, Parentheses)]
> parensExamples = [("(1)", Parentheses 1)
>                  ,("(17)", Parentheses 17)]

> parens :: Parser Parentheses
> parens = do
>     void $ char '('
>     e <- many1 digit
>     void $ char ')'
>     return (Parentheses (read e))

There is a new function: `void`. This might be familiar to you
already. This is used to ignore the result of the `char` parser, since
we are not interested in this value. You can also write the function
without `void`, but ghc will give you a warning if you have warnings
turned on.

One way of turning warnings on in ghci is to enter `:set -Wall` at the
ghci prompt.

> parens' :: Parser Parentheses
> parens' = do
>     char '('
>     e <- many1 digit
>     char ')'
>     return (Parentheses (read e))

```
*Main> :set -Wall
*Main> :l "VerySimpleExpressions.lhs"

...

FirstRealParsing.lhs:140:7: Warning:
    A do-notation statement discarded a result of type Char.
    Suppress this warning by saying "_ <- char '('",
    or by using the flag -fno-warn-unused-do-bind

FirstRealParsing.lhs:142:7: Warning:
    A do-notation statement discarded a result of type Char.
    Suppress this warning by saying "_ <- char ')'",
    or by using the flag -fno-warn-unused-do-bind

...

```

As you can see, another way to suppress the warning is to use
`_ <- char '('`.

One issue with this parser is that it doesn't handle whitespace:

```
*Main> regularParse parens "(1)"
Right (Parentheses 1)

*Main> regularParse parens "( 1)"
Left (line 1, column 2):
unexpected " "
expecting digit

*Main> regularParse parens "(1 )"
Left (line 1, column 3):
unexpected " "
expecting digit or ")"
```

We will look at this issue below.

= add

Now we will write a little parser to parse strings like 'a+b' where a
and b are numbers.

> data SingleAdd = SingleAdd Integer Integer
>                  deriving (Eq,Show)
>
> singleAddExamples :: [(String, SingleAdd)]
> singleAddExamples = [("1+2", SingleAdd 1 2)
>                     ,("101+202", SingleAdd 101 202)]

> add :: Parser SingleAdd
> add = do
>     e0 <- many1 digit
>     void $ char '+'
>     e1 <- many1 digit
>     return (SingleAdd (read e0) (read e1))

It has the same whitespace issues as the parens parser.

```
*Main> regularParse add "1+2"
Right (SingleAdd 1 2)

*Main> regularParse add "1 +2"
Left (line 1, column 2):
unexpected " "
expecting digit or "+"

```

= whitespace

Here is a parser which will skip zero or more whitespace characters.

> whitespace :: Parser ()
> whitespace = void $ many $ oneOf " \n\t"

We can use this to make our parsers handle whitespace better.

```
*Main> regularParse whitespace " "
Right ()
*Main> regularParse whitespace "  "
Right ()
*Main> regularParse whitespace "\t"
Right ()
*Main> regularParse whitespace " \n "
Right ()
*Main> regularParse whitespace ""
Right ()
```

Notice that it always succeeds.

Here is the parens parser rewritten with a common approach to
whitespace handling:

> parensW :: Parser Parentheses
> parensW = do
>     whitespace
>     void $ char '('
>     whitespace
>     e <- many1 digit
>     whitespace
>     void $ char ')'
>     whitespace
>     return (Parentheses (read e))

```
*Main> regularParse parensW "(1)"
Right (Parentheses 1)

*Main> regularParse parensW " (1)"
Right (Parentheses 1)

*Main> regularParse parensW " (1 )"
Right (Parentheses 1)

*Main> regularParse parensW " ( 1 ) "
Right (Parentheses 1)
```

Looks good.

In the original parsec documentation, one of the concepts mentioned is
the idea of 'lexeme' parsing. This is a style in which every token
parser should also consume and ignore any trailing whitespace.

This is a simple convention which with a bit of care allows skipping
whitespace exactly once wherever it needs to be skipped. To complete
the lexeme style, we should also always skip leading whitespace at the
top level only. This feels more elegant than spamming all the parsing
code with many calls to `whitespace`.

> lexeme :: Parser a -> Parser a
> lexeme p = do
>            x <- p
>            whitespace
>            return x

> parseWithWhitespace :: Parser a -> String -> Either ParseError a
> parseWithWhitespace p = parseWithEof wrapper
>   where
>     wrapper = do
>         whitespace
>         p


Here is the parens parser rewritten to use lexeme:

> parensL :: Parser Parentheses
> parensL = do
>     void $ lexeme $ char '('
>     e <- lexeme $ many1 digit
>     void $ lexeme $ char ')'
>     return (Parentheses (read e))

```
*Main> parseWithWhitespace parensL "(1)"
Right (Parentheses 1)

*Main> parseWithWhitespace parensL " (1)"
Right (Parentheses 1)

*Main> parseWithWhitespace parensL " ( 1)"
Right (Parentheses 1)

*Main> parseWithWhitespace parensL " ( 1 ) "
Right (Parentheses 1)
```

The `parseWithWhitespace` function can also use `(>>)` to make it a
bit shorter, `wrapper = whiteSpace >> p`.

Here is the shorter version of this function using `(>>)`:

> parseWithWhitespace' :: Parser a -> String -> Either ParseError a
> parseWithWhitespace' p = parseWithEof (whitespace >> p)

Try rewriting the SingleAdd parser to use `lexeme`, and test it out to
convince yourself that it skips whitespace correctly.

= simple expr

Now we are ready to write a parser which parses simple expressions
made from these components. Here is the data type for these
expressions.

> data SimpleExpr = Num Integer
>                 | Var String
>                 | Add SimpleExpr SimpleExpr
>                 | Parens SimpleExpr
>                   deriving (Eq,Show)

It's so simple that it is almost useless at the moment.

> simpleExprExamples :: [(String,SimpleExpr)]
> simpleExprExamples =
>     [("a", Var "a")
>     ,("1", Num 1)
>     ,("2 + 3", Add (Num 2) (Num 3))
>     ,("(42)", Parens (Num 42))]

TODO: some more complex examples

Here are all our component parsers with `lexeme`, and with the
`SimpleExpr` constructors:

> numE :: Parser SimpleExpr
> numE = do
>     n <- lexeme $ many1 digit
>     return $ Num $ read n

There doesn't seem to be a unique obviously correct place to put the
lexeme call in the var parser:

> varE :: Parser SimpleExpr
> varE = lexeme $ do
>     fc <- firstChar
>     rest <- many nonFirstChar
>     return $ Var (fc:rest)
>   where
>     firstChar = satisfy (\a -> isLetter a || a == '_')
>     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

Here is an alternative, with the call to lexeme in a different place,
but gives effectively the same function.

> varE' :: Parser SimpleExpr
> varE' = do
>     fc <- firstChar
>     rest <- lexeme $ many nonFirstChar
>     return $ Var (fc:rest)
>   where
>     firstChar = satisfy (\a -> isLetter a || a == '_')
>     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

> parensE :: Parser SimpleExpr
> parensE = do
>     void $ lexeme $ char '('
>     e <- lexeme $ many1 digit
>     void $ lexeme $ char ')'
>     return $ Parens $ Num $ read e

In the parens parser, we can reuse the `numE` parser like this:

> parensE' :: Parser SimpleExpr
> parensE' = do
>     void $ lexeme $ char '('
>     e <- numE
>     void $ lexeme $ char ')'
>     return $ Parens e

Here is the add parser using `numE` also.

> addE :: Parser SimpleExpr
> addE = do
>     e0 <- numE
>     void $ lexeme $ char '+'
>     e1 <- numE
>     return $ Add e0 e1

== choice

To combine these, we can use an operator called `(<|>)`:

> numOrVar :: Parser SimpleExpr
> numOrVar = numE <|> varE

It tries the first parser, and it if fails (without consuming any
input), it tries the second parser. More about the 'consuming input'
concept later.

Here is another way to write the numOrVar parser:

> numOrVar' :: Parser SimpleExpr
> numOrVar' = choice [numE,varE]

`choice` is just wrapper around `(<|>)`. You can choose which one to
use based on which is more readable in each particular case.

```
*Main> parseWithWhitespace numOrVar "a"
Right (Var "a")

*Main> parseWithWhitespace numOrVar "1"
Right (Num 1)

*Main> parseWithWhitespace numOrVar "!"
Left (line 1, column 1):
unexpected "!"
expecting digit
```

Here is the first version of the simpleExpr parser:

> simpleExpr :: Parser SimpleExpr
> simpleExpr = numE <|> varE <|> addE <|> parensE

```
*Main> parseWithWhitespace simpleExpr "12"
Right (Num 12)

*Main> parseWithWhitespace simpleExpr "aa"
Right (Var "aa")

*Main> parseWithWhitespace simpleExpr "1+2"
Left (line 1, column 2):
unexpected '+'
expecting digit or end of input

*Main> parseWithWhitespace simpleExpr "(1)"
Right (Parens (Num 1))

*Main> parseWithWhitespace simpleExpr "(aa)"
Left (line 1, column 2):
unexpected "a"
expecting digit

```

It works well for some of the parsers. One problem is that the `addE`
and `parensE` parsers don't parse general expressions as the
components, but just `numE`. Another problem is that the `addE`
doesn't work at all: the `numE` parser parses the first number, and
the `addE` parser is never tried. This is an example of `(<|>)` not
trying the second parser if the first parser succeeds, even if a later
alternative would consume more input or successfully parse the whole
input.

Let's try and rearrange the order:

> simpleExpr1 :: Parser SimpleExpr
> simpleExpr1 = addE <|> numE <|> varE <|> parensE

```
*Main> parseWithWhitespace simpleExpr1 "12"
Left (line 1, column 3):
unexpected end of input
expecting digit or "+"

*Main> parseWithWhitespace simpleExpr1 "aa"
Right (Var "aa")

*Main> parseWithWhitespace simpleExpr1 "1+2"
Right (Add (Num 1) (Num 2))

*Main> parseWithWhitespace simpleExpr1 "(1)"
Right (Parens (Num 1))

```

We swapped one problem for another. Let's fix this using the `try`
function.


> simpleExpr2 :: Parser SimpleExpr
> simpleExpr2 = try addE <|> numE <|> varE <|> parensE

```
*Main> parseWithWhitespace simpleExpr2 "12"
Right (Num 12)

*Main> parseWithWhitespace simpleExpr2 "aa"
Right (Var "aa")

*Main> parseWithWhitespace simpleExpr2 "1+2"
Right (Add (Num 1) (Num 2))

*Main> parseWithWhitespace simpleExpr2 "(1)"
Right (Parens (Num 1))
```

Now everything seems to work fine. The `try` function is very powerful
and easy to use, and can be used where in a more traditional parsing
approach you would have to use left factoring or something else.

The `try` function implements backtracking. When this is used with
`(<|>)`, it means that if the first parser fails, it will undo the
consumed input and carry on with the next option, instead of failing
completely. This works even if the `try` is nested deeply within the
first parser given to `(<|>)`.

`try` has its downsides (some of which we will see later), and I
usually try to minimise its use or eliminate it completely. I found I
often got into a complete mess when I used `try` too much when writing
parsers for something a little tricky like SQL, and that although
doing some left-factoring appeared at first to be tedious and appeared
to make the code less readable, I eventually decided that for me it
made the code more readable since what was happening was more
transparent.

Now we are going to fix this parser to parse arbitrarily nested
expressions. In a way, the method used will roughly mean we are left
factoring the `numE` and `addE` common prefix.

Here is the naive implementation:

> parensE3 :: Parser SimpleExpr
> parensE3 = do
>     void $ lexeme $ char '('
>     e <- simpleExpr3
>     void $ lexeme $ char ')'
>     return $ Parens e


> addE3 :: Parser SimpleExpr
> addE3 = do
>     e0 <- simpleExpr3
>     void $ lexeme $ char '+'
>     e1 <- simpleExpr3
>     return $ Add e0 e1


> simpleExpr3 :: Parser SimpleExpr
> simpleExpr3 = try addE3 <|> numE <|> varE <|> parensE3

If you run this parser, it will enter an infinite loop, since
`simpleExpr3` and `addE3` will keep calling each other recursively
without making any progress.

```
*Main> parseWithWhitespace simpleExpr3 "a+b"
  C-c Interrupted.
```
Let's try without `add`.

> parensE4 :: Parser SimpleExpr
> parensE4 = do
>     void $ lexeme $ char '('
>     e <- simpleExpr4
>     void $ lexeme $ char ')'
>     return $ Parens e

> simpleExpr4 :: Parser SimpleExpr
> simpleExpr4 = numE <|> varE <|> parensE4

```
*Main> parseWithWhitespace simpleExpr4 "a"
Right (Var "a")

*Main> parseWithWhitespace simpleExpr4 "1"
Right (Num 1)

*Main> parseWithWhitespace simpleExpr4 "(1)"
Right (Parens (Num 1))

*Main> parseWithWhitespace simpleExpr4 "((a))"
Right (Parens (Parens (Var "a")))

```

At least this part seems to work OK.

Let's try to stop the add parser from calling itself indirectly:

Here is a parameterized parens parser where we supply the nested
expression parser as an argument. This is used here to try to make the
code easier to follow and avoid rewriting this parser out again and
again in full.

> parensEN :: Parser SimpleExpr -> Parser SimpleExpr
> parensEN simpleExprImpl = do
>     void $ lexeme $ char '('
>     e <- simpleExprImpl
>     void $ lexeme $ char ')'
>     return $ Parens e

Here is a new parser, which parses expressions except add.

> term :: Parser SimpleExpr -> Parser SimpleExpr
> term simpleExprImpl = numE <|> varE <|> parensEN simpleExprImpl

> term5 :: Parser SimpleExpr
> term5 = term term5

> addE5 :: Parser SimpleExpr
> addE5 = do
>     e0 <- term5
>     void $ lexeme $ char '+'
>     e1 <- term5
>     return $ Add e0 e1

> simpleExpr5 :: Parser SimpleExpr
> simpleExpr5 = try addE5 <|> term5

```
*Main> parseWithWhitespace simpleExpr5 "1"
Right (Num 1)

*Main> parseWithWhitespace simpleExpr5 "a"
Right (Var "a")

*Main> parseWithWhitespace simpleExpr5 "(a)"
Right (Parens (Var "a"))

*Main> parseWithWhitespace simpleExpr5 "1+2"
Right (Add (Num 1) (Num 2))

*Main> parseWithWhitespace simpleExpr5 "1+a"
Right (Add (Num 1) (Var "a"))

*Main> parseWithWhitespace simpleExpr5 "(1+a)"
Right (Parens (Add (Num 1) (Var "a")))

*Main> parseWithWhitespace simpleExpr5 "1+a+b"
Left (line 1, column 4):
unexpected '+'
expecting end of input
```

Almost. Let's see what happens when the second `term` in `add` is
changed to the general expression parser.

> term6 :: Parser SimpleExpr
> term6 = term simpleExpr6

> addE6 :: Parser SimpleExpr
> addE6 = do
>     e0 <- term6
>     void $ lexeme $ char '+'
>     e1 <- simpleExpr6
>     return $ Add e0 e1

> simpleExpr6 :: Parser SimpleExpr
> simpleExpr6 = try addE6 <|> term6

```
*Main> parseWithWhitespace simpleExpr6 "a + b + c"
Right (Add (Var "a") (Add (Var "b") (Var "c")))
```

Maybe it looks like we've made it. But there is a problem. We've
parsed the + operator as if it has right associativity:

```
a + b + c -> a + (b + c)
```

But it should be left associative:

```
a + b + c -> (a + b) + c
```

Let's left factor the parsing and fix this:

> term7 :: Parser SimpleExpr
> term7 = term simpleExpr7

> simpleExpr7 :: Parser SimpleExpr
> simpleExpr7 = do
>     -- first parse a term
>     e <- term7
>     -- then see if it is followed by an '+ expr' suffix
>     maybeAddSuffix e
>   where
>     -- this function takes an expression, and parses a
>     -- '+ expr' suffix, returning an Add expression
>     -- it recursively calls itself via the maybeAddSuffix function
>     addSuffix e0 = do
>         void $ lexeme $ char '+'
>         e1 <- term7
>         maybeAddSuffix (Add e0 e1)
>     -- this is the wrapper for addSuffix, which adapts it so that if
>     -- addSuffix fails, it returns just the original expression
>     maybeAddSuffix e = addSuffix e <|> return e

```
*Main> parseWithWhitespace simpleExpr7 "a + b + c"
Right (Add (Add (Var "a") (Var "b")) (Var "c"))
```

Now the parser seems to work for everything it should.

There is a combinator function in Parsec we can use which abstracts
this sort of pattern, `chainl1`.

> simpleExpr8 :: Parser SimpleExpr
> simpleExpr8 = chainl1 term8 op
>   where
>     op = do
>         void $ lexeme $ char '+'
>         return Add
>     term8 = term simpleExpr8

How does this work? Here is the type of `chainl1`:

```haskell
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 term op = ...
```

The type of the Add constructor in pseudocode is:

```haskell
Add :: SimpleExpr -> SimpleExpr -> SimpleExpr
```

The `op` parser here now just parses the operator itself, i.e. '+'
(and not the second expression like our simpleExpr7 parser). The
return from the `op` function is a function which accepts two elements
and combines them using the appropriate operator representation. In
this case, the represenation is the `Add` constructor.

You can look at the source
<http://hackage.haskell.org/package/parsec-3.1.3/docs/src/Text-Parsec-Combinator.html#chainl1>
and see if you can understand how it works. If you can't work it out,
you could come back to it later when you have more experience writing
parsing code.

= Testing with the examples

TODO: write a little manual tester that accepts a parser and a list of
examples, and checks they all parse correctly.

= Testing with quickcheck

Let's see if we can check with quickcheck. It's a bit tricky testing
parsers in this way, but one way to do something useful is to generate
random asts, convert them to concrete syntax, parse them, and check
the result. We can write a simple 'pretty printer' to convert an ast
to concrete syntax.

== a pretty printer

TODO: a really simple pretty printer just pasting strings together, no
layout.

== the quick check code

TODO: write a quickcheck property and arbitary instance and show
running it at the ghci prompt


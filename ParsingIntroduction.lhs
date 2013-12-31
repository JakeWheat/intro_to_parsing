
This is an introduction to pasering with Haskell and Parsec.

Prerequisites: you should know some basic Haskell and have the Haskell
Platform installed (or GHC + Parsec installed in some other way).

This tutorial was written using GHC 7.6.3 and Parsec 3.1.3, which are
the versions which come with the Haskell Platform 2013.2.0.0.

TODO: Here is some other information on Parsec and Haskell:
links, tutorials on fp, section in rwh, lyah?, old parsec docs,
parsec docs on hackage, other parser combinator libs (uu, trifecta?)

This file is a Literate Haskell file, available here: TODO

You should download this lhs file, and follow along in your favourite
editor, and use ghci to experiment.

> import qualified Text.Parsec as P
> import Text.Parsec (anyChar, eof, manyTill, oneOf, many1, letter, char, choice, try, satisfy, digit, string, anyToken,chainl1,between)
> import qualified Text.Parsec.String as P (Parser)
> import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), (<|>), many)
> import Control.Monad (void, ap)
> import Control.Monad.Identity (Identity)
> import Data.Char (isLetter, isDigit)
> import qualified Text.Parsec.Expr as E

= Getting started

The first parser:

> oneChar :: P.Parser Char
> oneChar = anyChar

Whenever we write a parser which parses to a value of type a, we give
it the return type of P.Parser a. In this case, we parse a character
so the return type is P.Parser Char. The P.Parser type is in the
module Text.Parsec.String. We will cover this in more detail later.

You should always use a type signature with these parsers. Because the
Parsec is quite generalized, without the type GHC doesn't have enough
information to compile this code. Try commenting out the type
signature above and loading into ghci to see the error message.

Let's use this parser. Change to the directory where you saved this
.lhs file, and run ghci. Then type in ':l ParsingIntroduction.lhs'. You
can run the parser using a wrapper, enter the following at the ghci
prompt: 'regularParse oneChar "a"'.

Here is a transcript:

~~~~
$ ghci
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :l ParsingIntroduction.lhs
[1 of 1] Compiling Main             ( ParsingIntroduction.lhs, interpreted )
Ok, modules loaded: Main.
*Main> regularParse oneChar "a"
Right 'a'
~~~~

Here are some examples of running this parser on various input:

~~~~
*Main> regularParse oneChar "a"
Right 'a'
*Main> regularParse oneChar "b"
Right 'b'
*Main> regularParse oneChar "0"
Right '0'
*Main> regularParse oneChar " "
Right ' '
*Main> regularParse oneChar "\n"
Right '\n'
*Main> regularParse oneChar "aa"
Right 'a'
*Main> regularParse oneChar ""
Left (line 1, column 1):
unexpected end of input
*Main> regularParse oneChar " a"
Right ' '
~~~~

TODO: show in table form as well.

You can see that if there are no characters, we get an error.
Otherwise, it takes the first character and returns it, and throws
away any trailing characters. The details of the helper function
regularParse will come later.

Here are two alternatives to regularParse you can also use for
experimenting for the time being:

~~~~
*Main> regularParse oneChar "a"
Right 'a'
*Main> parseWithEof oneChar "a"
Right 'a'
*Main> parseWithLeftOver oneChar "a"
Right ('a',"")
*Main> *Main> regularParse oneChar ""
Left (line 1, column 1):
unexpected end of input
*Main> parseWithEof oneChar ""
Left (line 1, column 1):
unexpected end of input
*Main> parseWithLeftOver oneChar ""
Left (line 1, column 1):
unexpected end of input
*Main> regularParse oneChar "aa"
Right 'a'
*Main> parseWithEof oneChar "aa"
Left (line 1, column 2):
unexpected 'a'
expecting end of input
*Main> parseWithLeftOver oneChar "aa"
Right ('a',"a")
~~~~

TODO: put in table as well
more examples to make the behaviour more clear.

You can use these functions and ghci to experiment. Try running all
the parsers in ghci on various input strings as you work through the
document to get a good feel for all the different features. You can
also write the parsers inline in the function call, for example:

~~~~
*Main> regularParse (many1 digit) "1"
Right "1"
*Main> regularParse (many1 digit) "122"
Right "122"
~~~~

This can be used to quickly try out new ad hoc parsers.

= Text.Parsec.Char

Let's go through some of the functions in Text.Parsec.Char module from
the Parsec package. The haddock is avaialble here:
(http://hackage.haskell.org/package/parsec-3.1.3/docs/Text-Parsec-Char.html).

~~~~
satisfy :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Char
~~~~

This is the main primitive function in Parsec. This looks at the next
character from the current input, and if the function (Char -> Bool)
returns true for this character, it 'pops' it from the input and
returns it. The current position in the input string is tracked behind
the scenes.

Using our simplified parser type - P.Parser - the satisfy function's
type can be written like this:

~~~~
satisfy :: (Char -> Bool) -> P.Parser Char
~~~~

TODO: a few examples

Tip: if you look at the docs on hackage
(http://hackage.haskell.org/package/parsec-3.1.3/docs/Text-Parsec-Char.html),
you can view the source. Most of the functions in Text.Parsec.Char are
straightforward. You can see that the satisfy function is a little
more primitive than the other functions from the source. I'm going to
skip the explanation for the implementation of satisfy for now.

Here is the parser we used above in the oneChar parser, with a
simplified type:

~~~~
anyChar :: Parser Char
~~~~

If you look at the source via the haddock link above, you can see it
uses satisfy.

Here are some other simple wrappers of satisfy which use different
validation functions.

The char parses a specific character which you supply:

~~~~
char :: Char -> Parser Char
~~~~

TODO: examples

These parsers all parse single hardcoded characters

~~~~
space :: Parser Char
newline :: Parser Char
tab :: Parser Char
~~~~

They all return a Char. You should be able to guess what Char they
return, you can double check your intuition using ghci.

These parser all parse one character from a hardcoded set of
characters:

~~~~
upper :: Parser Char
lower :: Parser Char
alphaNum :: Parser Char
letter :: Parser Char
digit :: Parser Char
hexDigit :: Parser Char
octDigit :: Parser Char
~~~~

In these cases, the return value is less redundant.

TODO: examples, with wrong chars, multiple chars as well as matches

oneOf and noneOf parse any of the characters in the given list

~~~~
oneOf :: [Char] -> Parser Char
noneOf :: [Char] -> Parser Char
~~~~

TODO: examples

Here are the final functions in Text.Parsec.Char:

string matches a complete string, one character at a time. We will
skip the explanation of the implementation for now. TODO: implement
string directly in terms of satisfy, and quickcheck it.

~~~~
string :: String -> Parser String
~~~~

TODO: examples


here is another slight extension, which uses a combinator (skipMany)
if you look at the source. We will cover this combinator shortly.

~~~~
spaces :: Parser ()
~~~~

TODO: examples

= A couple of exes

Here are two exes which you can use to parse either a string or a file
to help you experiment.

TODO: make these into links

ParseString.lhs

ParseFile.lhs

You can experiment using ghci, or with a string on the command line,
or by putting the text to parse into a file and parsing that.

TODO: transcript of using these exes.

= Simple combinators

Let's create a very simple expression language:

> data SimpleExpr = Num Integer
>                 | Var String
>                 | Add SimpleExpr SimpleExpr
>                 | Parens SimpleExpr
>                   deriving (Eq,Show)

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

We can start by writing a parser for each ctor in turn.

=== Num

To parse a number, we need to parse one or more digits, and then read
the resulting string. We can use the combinator 'many1' to help with
this. We will also use do notation.

> num :: P.Parser SimpleExpr
> num = do
>     n <- many1 digit
>     return (Num (read n))

Let's try it out. TODO: examples

How does it work? First, we parse one or more (many1) digits (digit),
and give the result the name 'n'. Then we convert the string to an
integer using read, and wrap it in a Num constructor.

The many1 function's type looks like this:

~~~~
many1 :: P.Parser a -> P.Parser [a]
~~~~

It applies the parser given one or more times, returning the result.

TODO: example show what happens when you use 'many' instead of 'many1'

=== Var

For var, we have to decide on a syntax for the identifiers. Let's go
for a common choice: identifiers must start with a letter or
underscore, and then they can be followed by zero or more letters,
underscores or digits in any combination.

> var :: P.Parser SimpleExpr
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

> add :: P.Parser SimpleExpr
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

Tip: to use -Wall in ghci, enter the following at the prompt:

~~~~
*Main> :set -Wall
~~~~

Try it out, then replace the line

~~~~
    void $ char '+'
~~~~

with

~~~~
    char '+'
~~~~

And check you see the warning. Another way of avoiding the warning is
to write this:

~~~~
    _ <- char '+'
~~~~

=== parens

> parens :: P.Parser SimpleExpr
> parens = do
>     void $ char '('
>     e <- num
>     void $ char ')'
>     return (Parens e)

The same two issues from the 'add' parser apply here: whitespace and
lack of a general simple expression parser, so it just uses the num
parser again instead. Now we will tackle the whitespace issue.

== whitespace and lexeme parsing

Here is a parser which will skip 0 or more whitespace characters.

> whiteSpace :: P.Parser ()
> whiteSpace = void $ many $ oneOf " \n\t"

In the original parsec documentation, one of the concepts mentioned is
the idea of 'lexeme' parsing. This is a style in which every token
parser should also consume and ignore any trailing whitespace. This is
a simple convention which with a bit of care allows skipping
whitespace exactly once wherever it needs to be skipped. To complete
the lexeme style, we should also always skip leading whitespace at the
top level only.

> parseWithWhitespace :: P.Parser a -> String -> Either P.ParseError a
> parseWithWhitespace p = parseWithEof wrapper
>   where
>     wrapper = do
>         whiteSpace
>         p

the wrapper function can also use (>>) to make it a bit shorter:

~~~~
wrapper = whiteSpace >> p
~~~~

Here is a shorter version of this function using (>>):

> parseWithWhitespace' :: P.Parser a -> String -> Either P.ParseError a
> parseWithWhitespace' p = parseWithEof (whiteSpace >> p)

Here is the num parser rewritten in the lexeme style:

> num' :: P.Parser SimpleExpr
> num' = do
>     n <- many1 digit
>     whiteSpace
>     return (Num (read n))

TODO: examples parseWithEof num "1", " 1", "1 ", " 1 ", then
parserWithWhitespace num'

Here are the other functions in lexeme style.

> var' :: P.Parser SimpleExpr
> var' = do
>     fl <- firstChar
>     rest <- many nonFirstChar
>     whiteSpace
>     return (Var (fl:rest))
>   where
>     firstChar = satisfy (\a -> isLetter a || a == '_')
>     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

> add' :: P.Parser SimpleExpr
> add' = do
>     e0 <- num'
>     void $ char '+'
>     whiteSpace
>     e1 <- num'
>     return (Add e0 e1)

> parens' :: P.Parser SimpleExpr
> parens' = do
>     void $ char '('
>     whiteSpace
>     e <- num'
>     void $ char ')'
>     whiteSpace
>     return (Parens e)

In this style, you have to be slightly careful to make sure you call
whitespace at the right points.

Let's try and implement the simpleExpr parser. We can use a function
called 'choice':

> numOrVar :: P.Parser SimpleExpr
> numOrVar = choice [num', var']

It tries each parser one at a time, finishing with the first one that
succeeds. There are some more details about this later on.

Here is another way to write the numOrVar parser:

> numOrVar' :: P.Parser SimpleExpr
> numOrVar' = num' <|> var'

In general, you can write 'choice [p0, p1, p2, ...]' as 'p0 <|> p1 <|>
p2 <|> ...'. I think for two choices, sometimes <|> is more readable,
but a lot of the time I prefer 'choice' since the layout of the source
code is better.

TODO: a bunch of examples

Here is the first version of the simpleExpr parser:

> simpleExpr :: P.Parser SimpleExpr
> simpleExpr = choice [num', var', add', parens']

TODO: a bunch of examples

It works well for some of the parsers, but fails with add': the num'
always partially succeeds first, then fails, so the add' is never
tried. We can rearrange this parser like this:

> simpleExpr' :: P.Parser SimpleExpr
> simpleExpr' = choice [add', num', var', parens']

TODO: examples again

We have another problem now: when we start parsing the add', see a
num, then fail, it gives up completely. This is because the 'choice'
function (and <|>) will only try the next parser if the parser that
failed consumed no input before failing.

TODO: show examples with satify and choice on 1 char match/no match,
and two char match/no match.

Here is one way to fix it:

> simpleExpr'' :: P.Parser SimpleExpr
> simpleExpr'' = choice [try add', num', var', parens']

The try function implements backtracking. When this is used in a
choice like this, it means that if the add' parser fails, it will undo
the consumed input and carry on with the next option, instead of
failing completely. If there is another place using try higher up in
the call stack, then we will continue there, otherwise the whole parse
will fail immediately.

The same happens with <|>, we can implement the simpleExpr parser like
this also:

> simpleExpr''' :: P.Parser SimpleExpr
> simpleExpr''' = try add' <|> num' <|> var' <|> parens'

TODO: show the examples all working

Now we can make 'parens' and 'add' use a general simple expression
parser. Parens is simple:

> parens'' :: P.Parser SimpleExpr
> parens'' = do
>     void $ char '('
>     whiteSpace
>     e <- simpleExpr'''
>     void $ char ')'
>     whiteSpace
>     return (Parens e)

There is a problem implementing 'add' in the same way:

> add'' :: P.Parser SimpleExpr
> add'' = do
>     e0 <- simpleExpr'''
>     void $ char '+'
>     whiteSpace
>     e1 <- simpleExpr'''
>     return (Add e0 e1)

It will never return since it calls simpleExpr''' which calls add''
again.

Let's look at another problem:

*Main> parseWithWhitespace simpleExpr''' " 1 + 1 + 1"
Left (line 1, column 8):
unexpected '+'
expecting end of input

Our parser will only parse one operator, and not a chain of them.

Here is one way to solve it:

> simpleExpr4 :: P.Parser SimpleExpr
> simpleExpr4 = do
>     e <- factor
>     maybeAddSuffix e
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 = do
>         void $ char '+'
>         whiteSpace
>         e1 <- factor
>         maybeAddSuffix (Add e0 e1)
>     factor =  num' <|> var' <|> parens'

TODO: explain how this works in much more detail

This forces our Add operator to be left associative. This also solves
the previous problem with add calling simpleExpr recursively. There is
lots of discussion about these issues in Parsing theory documents you
can find online, etc..

= general simple expression parser

Here is the all the parser code written out again for clarity.

> numD :: P.Parser SimpleExpr
> numD = do
>     n <- many1 digit
>     whiteSpace
>     return $ Num $ read n

> varD :: P.Parser SimpleExpr
> varD = do
>     fl <- firstChar
>     rest <- many nonFirstChar
>     whiteSpace
>     return $ Var (fl:rest)
>   where
>     firstChar = satisfy (\a -> isLetter a || a == '_')
>     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

> parensD :: P.Parser SimpleExpr
> parensD = do
>     void $ char '('
>     whiteSpace
>     e <- simpleExprD
>     void $ char ')'
>     whiteSpace
>     return $ Parens e

> simpleExprD :: P.Parser SimpleExpr
> simpleExprD = do
>     e <- factor
>     maybeAddSuffix e
>   where
>     maybeAddSuffix e =
>         choice [addSuffix e
>                ,return e]
>     addSuffix e0 = do
>         void $ char '+'
>         whiteSpace
>         e1 <- factor
>         maybeAddSuffix (Add e0 e1)
>     factor = numD <|> varD <|> parensD


== Testing with the examples

== Testing with quickcheck

=== a pretty printer

=== the quick check code

= tidying things up

Now we can go back over the code already written, and make it much
more concise, whilst also trying to make it more readable.

Here is the basic pattern behind a lot of this. Here is a function
which takes a constructor and two parsers for the two arguments for
the constructor. It parses the two arguements, then applies the
constructor to the results:

> myParser1 :: (a -> b -> c) -> P.Parser a -> P.Parser b -> P.Parser c
> myParser1 ctor pa pb = do
>     a <- pa
>     b <- pb
>     return $ ctor a b

TODO: concrete example

This can be rewritten without the do syntactic sugar like this:

> myParser2 :: (a -> b -> c) -> P.Parser a -> P.Parser b -> P.Parser c
> myParser2 ctor pa pb =
>     pa >>= \a -> pb >>= \b -> return $ ctor a b

And this can be rewritten like this:

> myParser3 :: (a -> b -> c) -> P.Parser a -> P.Parser b -> P.Parser c
> myParser3 ctor pa pb = ctor `fmap` pa `ap` pb

(This uses functions from Applicative instead of Monad). We replace
the use of >>= with 'fmap' and 'ap'. This isn't always possible, but
it often is.

Here is the version using the operator versions (fmap changed to <$>,
and ap changed to <*>). These two operators are just alternative
spellings of fmap and ap.

> myParser4 :: (a -> b -> c) -> P.Parser a -> P.Parser b -> P.Parser c
> myParser4 ctor pa pb = ctor <$> pa <*> pb

This style takes less typing, and is often much simpler to write and
read when you are familiar with it.

This pattern 'scales', you can use:

Ctor <$> pa

for a single argument constructor. This might also be familiar to you
as

fmap Ctor pa

or

Ctor `fmap` pa

All of which mean the same thing, just slightly different spellings.

This can also be written using Monad operators:

pa >>= liftM ctor

or

liftM ctor =<< pa

(liftM is in Control.Monad)

These effectively mean the same thing as the previous versions with
fmap and <$>.

You can use

Ctor <$> pa <*> pb <*> pc

for three args, and so on. So you use <$> between the pure Ctor and
the first arg, then <*> between each subsequent arg.

Let's go over the simple expression parsers and try to rewrite them
using this style. We will see a few other new functions. I will take a
lot of time over the changes to the source.

== num

Here was the old parser.

> numA0 :: P.Parser SimpleExpr
> numA0 = do
>    n <- many1 digit
>    whiteSpace
>    return $ Num $ read n

Let's rewrite it in steps. The first step is to move the 'read' to the
first line using fmap (fmap is spelt <$> here).

> numA1 :: P.Parser SimpleExpr
> numA1 = do
>     n <- read <$> many1 digit
>     whiteSpace
>     return $ Num n

Now we can move the Num constructor:

> numA2 :: P.Parser SimpleExpr
> numA2 = do
>     n <- Num . read <$> many1 digit
>     whiteSpace
>     return n

How can we get rid of the whitespace. Here is an additional operator (<*), which can be used:

a <* b

is equivalent to

do
    x <- a
    void b
    return x

Here it is in use in the function.

> numA3 :: P.Parser SimpleExpr
> numA3 = do
>     n <- Num . read <$> many1 digit <* whiteSpace
>     return n

Now we can apply the usual monad rewrite laws:

> numA4 :: P.Parser SimpleExpr
> numA4 = Num . read <$> many1 digit <* whiteSpace

In more industrial parser code, I would usually write some
'tokenization' parsers separately like this:

> integerA5 :: P.Parser Integer
> integerA5 = read <$> many1 digit <* whiteSpace

Then the num expression parser looks like this:

> numA5 :: P.Parser SimpleExpr
> numA5 = Num <$> integerA5

and we get a integer parser which we can reuse if we need to parse an
integer in another context.

== var

here is the old var parser:

varD :: P.Parser SimpleExpr
varD = do
    fl <- firstChar
    rest <- many nonFirstChar
    whiteSpace
    return $ Var (fl:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

The first thing we can do is to make the firstChar and nonFirstChar a
little easier to read:

> varA0 :: P.Parser SimpleExpr
> varA0 = do
>     fl <- firstChar
>     rest <- many nonFirstChar
>     whiteSpace
>     return $ Var (fl:rest)
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

Now we can lift the (:) using the Applicative operators.

> varA1 :: P.Parser SimpleExpr
> varA1 = do
>     i <- iden
>     return $ Var i
>   where
>     iden = (:) <$> firstChar <*> many nonFirstChar <* whiteSpace
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

And tidy it up using <$> with the Var constructor:

> varA2 :: P.Parser SimpleExpr
> varA2 = Var <$> iden
>   where
>     iden = (:) <$> firstChar <*> many nonFirstChar <* whiteSpace
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

It looks almost like a grammar description now.

== parens

Here is the starting point:

parensD :: P.Parser SimpleExpr
parensD = do
    void $ char '('
    whiteSpace
    e <- simpleExprD
    void $ char ')'
    whiteSpace
    return $ Parens e

Here you can see that there is a (*>) which works in the opposite
direction to (<*), and that these functions - (<*), (*>) - can be
chained. The precendence of these operators means that we have to use
some extra parentheses (!) here:

> parensA0 :: P.Parser SimpleExpr
> parensA0 = Parens <$> (char '(' *> whiteSpace *> simpleExprD)
>                   <* char ')' <* whiteSpace

Hopefully, learning more about parsing and implementing parsers will
give you a more subtle understanding of when we have to put
parentheses in Haskell code to make it parse correctly. Later, I will
show you how the different precedence of (*>) and (>>) - which
essentially do the same thing - means that in many situations using
(>>) instead of (*>) means you can leave out some parentheses. I'm not
sure if this is clever or not. HLint tells you something different to
received wisdom: HLint says always get rid of redundant parentheses,
and received wisdom says always put them in so you know exactly how
some code parses without having to know the operator precendences...
TODO: rewrite this text it is a mess

== simple expr

Here is the old version

simpleExprD :: P.Parser SimpleExpr
simpleExprD = do
    e <- factor
    maybeAddSuffix e
  where
    maybeAddSuffix e = addSuffix e <|> return e
    addSuffix e0 = do
        void $ char '+'
        whiteSpace
        e1 <- factor
        maybeAddSuffix (Add e0 e1)
    factor = numD <|> varD <|> parensD

Start with the function body, convert to point free style.

> simpleExprA0 :: P.Parser SimpleExpr
> simpleExprA0 = factor >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 = do
>         void $ char '+'
>         whiteSpace
>         e1 <- factor
>         maybeAddSuffix (Add e0 e1)
>     factor = numD <|> varD <|> parensD

Now rewrite the main part of the addSuffix function:

> simpleExprA1 :: P.Parser SimpleExpr
> simpleExprA1 = factor >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 = do
>         e1 <- char '+' *> whiteSpace *> factor
>         maybeAddSuffix (Add e0 e1)
>     factor = numD <|> varD <|> parensD

now combine the Add ctor call into one line

> simpleExprA2 :: P.Parser SimpleExpr
> simpleExprA2 = factor >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 = do
>         ae <- Add e0 <$> (char '+' *> whiteSpace *> factor)
>         maybeAddSuffix ae
>     factor = numD <|> varD <|> parensD

now simplify the addSuffix function to make it point free.

> simpleExprA3 :: P.Parser SimpleExpr
> simpleExprA3 = factor >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 =
>         Add e0 <$> (char '+' *> whiteSpace *> factor)
>         >>= maybeAddSuffix
>     factor = numD <|> varD <|> parensD

TODO: use chainl here and an additional step. Alter the code lower
down to match this change.

== summary

Here is the finished job for all the simple expression code.

> numA' :: P.Parser SimpleExpr
> numA' = Num . read <$> many1 digit <* whiteSpace

> varA' :: P.Parser SimpleExpr
> varA' = Var <$> ((:) <$> firstChar <*> many nonFirstChar <* whiteSpace)
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

> parensA' :: P.Parser SimpleExpr
> parensA' = Parens <$> (char '(' *> whiteSpace *>
>                        simpleExprA'
>                        <* char ')' <* whiteSpace)

> simpleExprA' :: P.Parser SimpleExpr
> simpleExprA' = factor >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 =
>         (Add e0 <$> (char '+' *> whiteSpace *> factor)) >>= maybeAddSuffix
>     factor = numA' <|> varA' <|> parensA'

Here a version with the separate token parsers.

The token parsers:

> identifier :: P.Parser String
> identifier = (:) <$> firstChar <*> many nonFirstChar <* whiteSpace
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

> integer :: P.Parser Integer
> integer = read <$> many1 digit <* whiteSpace

> symbol :: Char -> P.Parser ()
> symbol c = void (char c <* whiteSpace)

Here is another little helper function. It barely pays its way in this
short example, but even though it is only used once, I think it is
worth it to make the code clearer.

> betweenParens :: P.Parser a -> P.Parser a
> betweenParens p = symbol '(' *> p <* symbol ')'

The expression parsers:

> numA :: P.Parser SimpleExpr
> numA = Num <$> integer

> varA :: P.Parser SimpleExpr
> varA = Var <$> identifier

> parensA :: P.Parser SimpleExpr
> parensA = Parens <$> betweenParens simpleExprA

> simpleExprA :: P.Parser SimpleExpr
> simpleExprA = factor >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 = (Add e0 <$> (symbol '+' *> factor)) >>= maybeAddSuffix
>     factor = numA <|> varA <|> parensA

Splitting the lexer parser layer out means that we have one place
where we have to remember to add the ignore whitespace suffixes, and I
think makes the code a bit simpler to follow.

Here is a version of simpleExprA using the chainl1 function from
Text.Parsec.Combinator:

> simpleExprAC :: P.Parser SimpleExpr
> simpleExprAC = chainl1 factor op
>   where
>     op = symbol '+' *> return Add
>     factor = numA <|> varA <|> parensA

The chainl1 simplified type is like this:

~~~~
Parser a -> Parser (a -> a -> a) -> Parser a
~~~~

You pass it the function to parse a single element, then a parser
which parses the operator concrete syntax, and returns a function
which combines two elements using the operator abstract syntax, and it
does all the work. So the 'op' function here parses a plus symbol,
then returns the Add ctor, whose type signature is what we want:

~~~~
Add :: SimpleExpr -> SimpleExpr -> SimpleExpr
~~~~

If you look at the source of chainl1, it is more or less the code
above in simpleExprA factored out as a separate function, but written
in a different style.

= Text.Parsec.Combinator

Now we will go through the Text.Parsec.Combinator module. I have
mostly just written the original type signature, the simplified type
signature, and reproduced the haddock documentation comment for each
parser. Some have additional comments, and each has some example
usage.

TODO: examples for each, remove the 'Source', replace the type
signatures with simplified ones.

~~~~
choice :: [Parser a] -> Parser a
~~~~

Haddock: choice ps tries to apply the parsers in the list ps in order, until
one of them succeeds. Returns the value of the succeeding parser.

We've already seen this one in use.

~~~~
count :: Int -> Parser a -> Parser [a]
~~~~

Haddock: count n p parses n occurrences of p. If n is smaller or equal
to zero, the parser equals to return []. Returns a list of n values
returned by p.

TODO: examples

~~~~
between :: Parser open -> Parser close -> Parser a -> Parser a

Haddock: between open close p parses open, followed by p and close. Returns the
value returned by p.

~~~~
  braces  = between (symbol "{") (symbol "}")
~~~~

We can replace the betweenParens using this:

> betweenParens' :: P.Parser a -> P.Parser a
> betweenParens' p = between (symbol '(') (symbol ')') p

It hardly seems worth it, but if you are already familiar with
between, then it is quicker to read, which helps in a situation like
when you are in a rush and coming back to code you wrote 3 years ago,
for instance.


~~~~
option :: a -> Parser a -> Parser a
~~~~

Haddock: option x p tries to apply parser p. If p fails without
consuming input, it returns the value x, otherwise the value returned
by p.

The example given is written in a different style to this document:

~~~~
  priority  = option 0 (do{ d <- digit
                          ; return (digitToInt d)
                          })
~~~~

Exercise: write the digitToInt function and rewrite this example using
<$>.

~~~~
optionMaybe :: Parser a -> Parser (Maybe a)
~~~~

optionMaybe p tries to apply parser p. If p fails without consuming
input, it returns Nothing, otherwise it returns Just the value returned
by p.

~~~~
optional :: Parser a -> Parser ()
~~~~

optional p tries to apply parser p. It will parse p or nothing. It
only fails if p fails after consuming input. It discards the result of
p.

~~~~
skipMany1 :: Parser a -> Parser ()
~~~~

skipMany1 p applies the parser p one or more times, skipping its result.

~~~~
many1 :: Parser a -> Parser [a]
~~~~

many1 p applies the parser p one or more times. Returns a list of the
returned values of p.

~~~~
  word  = many1 letter
~~~~

~~~~
sepBy :: Parser a -> Parser sep -> Parser [a]
~~~~

sepBy p sep parses zero or more occurrences of p, separated by
sep. Returns a list of values returned by p.

~~~~
  commaSep p  = p `sepBy` (symbol ",")
~~~~

~~~~
sepBy1 :: Parser a -> Parser sep -> Parser [a]
~~~~

sepBy1 p sep parses one or more occurrences of p, separated by
sep. Returns a list of values returned by p.

~~~~
endBy :: Parser a -> Parser sep -> Parser [a]
~~~~

endBy p sep parses zero or more occurrences of p, seperated and ended
by sep. Returns a list of values returned by p.

~~~~
   cStatements  = cStatement `endBy` semi
~~~~

~~~~
endBy1 :: Parser a -> Parser sep -> Parser [a]
~~~~

endBy1 p sep parses one or more occurrences of p, seperated and ended
by sep. Returns a list of values returned by p.

~~~~
sepEndBy :: Parser a -> Parser sep -> Parser [a]
~~~~

sepEndBy p sep parses zero or more occurrences of p, separated and
optionally ended by sep, ie. haskell style statements. Returns a list
of values returned by p.

~~~~
  haskellStatements  = haskellStatement `sepEndBy` semi
~~~~

~~~~
sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
~~~~

sepEndBy1 p sep parses one or more occurrences of p, separated and
optionally ended by sep. Returns a list of values returned by p.

~~~~
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
~~~~

chainl p op x parser zero or more occurrences of p, separated by
op. Returns a value obtained by a left associative application of all
functions returned by op to the values returned by p. If there are
zero occurrences of p, the value x is returned.

~~~~
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
~~~~

chainl1 p op x parser one or more occurrences of p, separated by op
Returns a value obtained by a left associative application of all
functions returned by op to the values returned by p. . This parser
can for example be used to eliminate left recursion which typically
occurs in expression grammars.

TODO: reimplement the Add suffix parser using chainl1

~~~~
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
~~~~

chainr p op x parser zero or more occurrences of p, separated by op
Returns a value obtained by a right associative application of all
functions returned by op to the values returned by p. If there are no
occurrences of p, the value x is returned.

~~~~
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
~~~~

chainr1 p op x parser one or more occurrences of |p|, separated by op
Returns a value obtained by a right associative application of all
functions returned by op to the values returned by p.

~~~~
eof :: Parser ()
~~~~

This parser only succeeds at the end of the input. This is not a
primitive parser but it is defined using notFollowedBy.

~~~~
  eof  = notFollowedBy anyToken <?> "end of input"
~~~~

The (<?>) operator is used for error messages. We will come back to
error messages after writing the basic SQL parser.

~~~~
notFollowedBy :: Show a => Parser a -> Parser ()
~~~~

notFollowedBy p only succeeds when parser p fails. This parser does
not consume any input. This parser can be used to implement the
'longest match' rule. For example, when recognizing keywords (for
example let), we want to make sure that a keyword is not followed by a
legal identifier character, in which case the keyword is actually an
identifier (for example lets). We can program this behaviour as
follows:

~~~~
  keywordLet  = try (do{ string "let"
                       ; notFollowedBy alphaNum
                       })
~~~~

~~~~
manyTill :: Parser a -> Parser end -> Parser [a]
~~~~

manyTill p end applies parser p zero or more times until parser end
succeeds. Returns the list of values returned by p. This parser can be
used to scan comments:

~~~~
  simpleComment   = do{ string "<!--"
                      ; manyTill anyChar (try (string "-->"))
                      }
~~~~

Note the overlapping parsers anyChar and string "-->", and therefore
the use of the try combinator.

~~~~
lookAhead :: Parser a -> Parser a
~~~~

lookAhead p parses p without consuming any input.

If p fails and consumes some input, so does lookAhead. Combine with
try if this is undesirable.

~~~~
anyToken :: Parser Char
~~~~

The parser anyToken accepts any kind of token. It is for example used
to implement eof. Returns the accepted token.

= Control.Applicative

Here are the functions from Applicative that are used:

(<$>), (<*>), (<$), (<*), (*>), (<|>), many

We've already seen all of these, except (<$). This is often used to
parse a keyword and return a no argument constructor:

> data Something = Type1 | Type2 | Type3

> something :: P.Parser Something
> something = choice [Type1 <$ string "type1"
>                    ,Type2 <$ string "type2"
>                    ,Type3 <$ string "type3"]

There is also (<**>) which is (<*>) with the arguments flipped. I
don't use this one much.

TODO: double check using these from Parsec instead of
Control.Applicative: possible performance implications?

= Boilerplate

Here are the testing functions which were used earlier:

The basic parse function: this is a pretty simple wrapper. The parse
function from parsec just adds a filename to use in parse errors,
which is set as the empty string here.

> regularParse :: P.Parser a -> String -> Either P.ParseError a
> regularParse p = P.parse p ""

'parse' is a basic function in the family of functions for running
parsers in Parsec. You can compose the parser functions in the Parser
monad, then run the top level function using 'parse' and get back an
'Either ParserError a' as the result. There are a few alternatives to
'parse' in Parsec, mostly when you are using a more general parser
type instead of 'Parser a' (which is an alias for 'ParsecT String ()
Identity a'). Have a look in the Text.Parsec.Prim module for these
<http://hackage.haskell.org/package/parsec-3.1.3/docs/Text-Parsec-Prim.html>.

This function will run the parser, but additionally fail if it doesn't
consume all the input.

> parseWithEof :: P.Parser a -> String -> Either P.ParseError a
> parseWithEof p = P.parse (p <* eof) ""

This function will apply the parser, then also return any left over
input which wasn't parsed.

> parseWithLeftOver :: P.Parser a -> String -> Either P.ParseError (a,String)
> parseWithLeftOver p = P.parse ((,) <$> p <*> leftOver) ""
>   where leftOver = manyTill anyToken eof

TODO: what happens when you use 'many anyToken <* eof' variations
instead?

Here is the main testing function used, which includes ignoring prefix
whitespace.

> parseWithWSEof :: P.Parser a -> String -> Either P.ParseError a
> parseWithWSEof p = parseWithEof (whiteSpace *> p)

= P.Parser

The definition of P.Parser and a partial explanation of the full type
signature.

~~~~
type Parser = Parsec String ()
~~~~

This means that a function returning Parser a parses from a String
with () as the initial state.

The Parsec type is defined like this:

~~~~
type Parsec s u = ParsecT s u Identity
~~~~

ParsecT is a monad transformer, I think it is the primitive one in the
Parsec library, and the 'Parsec' type is a type alias which sets the
base monad to be Identity.

Here is the haddock for the ParsecT type:

ParsecT s u m a is a parser with stream type s, user state type u,
underlying monad m and return type a.

The full types that you see like this:

satisfy :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Char

refer to the same things (stream type s, user state type u, underlying
monad m).

We are using String as the stream type (i.e. the input type), () as
the user state type (this effectively means no user state, since ()
only has one value), and the underlying monad is Identity: we are
using no other underlying monad, so 'Parser a' expands to:

~~~~
ParsecT String () Identity a
~~~~

I.e. the source is String, the user state is (), and the underlying monad
is Identity.

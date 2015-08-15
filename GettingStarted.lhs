
[[getting-started]]
= Getting started

This is an introduction to parsing with Haskell and Parsec.

Prerequisites: you should know some basic Haskell and have GHC and
cabal-install installed (installing the Haskell Platform will give you
this).

This tutorial was originally written using GHC 7.6.3 and Parsec 3.1.3,
which are the versions which come with the Haskell Platform
2013.2.0.0. It should also work fine with GHC 7.8.4 and GHC 7.10.2 and
through to at least the latest release of Parsec 3.1.x.

This tutorial was written using Literate Haskell files available here:
link:https://github.com/JakeWheat/intro_to_parsing[].

I recommend you download them all, and follow along in your favourite
editor, and use GHCi to experiment. To download the intro_to_parsing
files, use:

```
git clone https://github.com/JakeWheat/intro_to_parsing.git
```

Here are the imports.

> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Char (anyChar)
> import Text.Parsec.String.Char
> import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
> import Data.Char
> import Text.Parsec.String.Combinator (many1)

== First parser

The first parser:

```
anyChar :: Parser Char
```

This parser is in the module `Text.Parsec.Char`. There is a wrapper in
this tutorial's project, `Text.Parsec.String.Char`, which gives this
function a simplified type.

Whenever we write a parser which parses to a value of type `a`, we give
it the return type of `Parser a`. In this case, we parse a character
so the return type is `Parser Char`. The `Parser` type itself is in
the module `Text.Parsec.String`. We will cover this in more detail
later.

Let's use this parser. I will assume you have GHC and cabal-install
installed (which provides the 'cabal' executable) and both are in your
PATH. The Haskell Platform is one way that provides this.

Change to the directory where you downloaded the intro_to_parsing
source files (which will contain the GettingStarted.lhs file). Then
you can set up a cabal sandbox and be ready to work with the code by
running the following commands in that directory:

```
cabal update
cabal sandbox init
cabal install parsec
cabal repl
```

Now you will get the ghci prompt. Type in ':l GettingStarted.lhs'. You
can run the parser using a wrapper, enter the following at the ghci
prompt: `regularParse anyChar "a"`.

Here is a transcript of running ghci via 'cabal repl':

```
$ cabal repl
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :l GettingStarted.lhs
[1 of 5] Compiling Text.Parsec.String.Parsec ( Text/Parsec/String/Parsec.hs, interpreted )
[2 of 5] Compiling Text.Parsec.String.Combinator ( Text/Parsec/String/Combinator.hs, interpreted )
[3 of 5] Compiling Text.Parsec.String.Char ( Text/Parsec/String/Char.hs, interpreted )
[4 of 5] Compiling FunctionsAndTypesForParsing ( FunctionsAndTypesForParsing.lhs, interpreted )
[5 of 5] Compiling Main             ( GettingStarted.lhs, interpreted )
Ok, modules loaded: FunctionsAndTypesForParsing, Text.Parsec.String.Char, Text.Parsec.String.Combinator, Main, Text.Parsec.String.Parsec.
*Main> regularParse anyChar "a"
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package bytestring-0.10.0.2 ... linking ... done.
Loading package containers-0.5.0.0 ... linking ... done.
Loading package binary-0.5.1.1 ... linking ... done.
Loading package transformers-0.4.3.0 ... linking ... done.
Loading package mtl-2.2.1 ... linking ... done.
Loading package text-1.2.1.3 ... linking ... done.
Loading package parsec-3.1.9 ... linking ... done.
Right 'a'
*Main>
```

You can exit ghci by entering ':quit' or using Ctrl-d. From now on, to
start ghci again, you can just change to the directory with
GettingStarted.lhs and run 'cabal repl'. ghci should have readline
support so you can browse through your command history using up and
down arrow, etc.

This is the type of `regularParse`. It is wrapper which takes a parser
function such as anyChar, and wraps it so you can parse a string to
either a parse error, or the return value from your parser function:

```
regularParse :: Parser a -> String -> Either ParseError a
```


Here are some examples of running this parser on various input:

```
*Main> regularParse anyChar "a"
Right 'a'

*Main> regularParse anyChar "b"
Right 'b'

*Main> regularParse anyChar "0"
Right '0'

*Main> regularParse anyChar " "
Right ' '

*Main> regularParse anyChar "\n"
Right '\n'

*Main> regularParse anyChar "aa"
Right 'a'

*Main> regularParse anyChar ""
Left (line 1, column 1):
unexpected end of input

*Main> regularParse anyChar " a"
Right ' '
```


You can see that if there are no characters, we get an error.
Otherwise, it takes the first character and returns it, and throws
away any trailing characters. The details of the helper function
`regularParse` will come later.

Here are two alternatives to `regularParse` you can also use for
experimenting for the time being:

```
parseWithEof :: Parser a -> String -> Either ParseError a
```

```
parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
```

These can be useful when you are not sure if your parser is consuming
all your input string or not. The eof parser will error if you haven't
consumed all the input, and the leftover parser can instead tell you
what was not consumed from the input.

```
*Main> regularParse anyChar "a"
Right 'a'

*Main> parseWithEof anyChar "a"
Right 'a'

*Main> parseWithLeftOver anyChar "a"
Right ('a',"")

*Main> *Main> regularParse anyChar ""
Left (line 1, column 1):
unexpected end of input

*Main> parseWithEof anyChar ""
Left (line 1, column 1):
unexpected end of input

*Main> parseWithLeftOver anyChar ""
Left (line 1, column 1):
unexpected end of input

*Main> regularParse anyChar "aa"
Right 'a'

*Main> parseWithEof anyChar "aa"
Left (line 1, column 2):
unexpected 'a'
expecting end of input

*Main> parseWithLeftOver anyChar "aa"
Right ('a',"a")

*Main> parseWithLeftOver anyChar "abc"
Right ('a',"bc")

```

You can use these functions and ghci to experiment. Try running all
the parsers in ghci on various input strings as you work through the
document to get a good feel for all the different features. Tip: you
can also write the parsers inline in the function call, for example:

```
*Main> regularParse (many1 digit) "1"
Right "1"

*Main> regularParse (many1 digit) "122"
Right "122"
```

This can be used to quickly try out new ad hoc parsing code.

== Type signatures

The real Parsec functions have quite complex type signatures. This
makes a lot of things very tricky before you understand them, and can
make some of the error messages you'll see really difficult to
understand. I've created some wrapper modules, which set the types of
all the functions from Parsec we use to be much more restricted. This
will make the types easy to understand, and reduce the amount of
tricky to understand compiler errors you get. You can use this
approach when writing your own parser code with Parsec. These wrapper
modules are created with the following name pattern:
`Text.Parsec.Char` -> `Text.Parsec.String.Char`.

Later on, we will look at the general types in more detail.

== Text.Parsec.Char

Let's go through some of the functions in `Text.Parsec.Char` module from
the Parsec package. The haddock is available here:
<http://hackage.haskell.org/package/parsec-3.1.3/docs/Text-Parsec-Char.html>.

Here is the `satisfy` function, with its full type signature.

```
satisfy :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Char
```

This is one of the main primitive functions in Parsec. This looks at
the next character from the current input, and if the function (`Char
-> Bool`) returns true for this character, it 'pops' it from the input
and returns it. In this way, the current position in the input string
is tracked behind the scenes.

In the simplified type wrappers, the `satisfy` function's type is this:

```
satisfy :: (Char -> Bool) -> Parser Char
```

This makes it a bit clearer what it is doing. All the functions in
`Text.Parsec.Char` are reproduced in the local `Text.Parsec.String.Char`
module with simplified types
(<https://github.com/JakeWheat/intro_to_parsing/blob/master/Text/Parsec/String/Char.hs>).

Here are some examples of satisfy in action.

```
*Main> parseWithEof (satisfy (=='a')) "a"
Right 'a'

*Main> parseWithEof (satisfy (=='b')) "a"
Left (line 1, column 1):
unexpected "a"

*Main> parseWithEof (satisfy (`elem` "abc")) "a"
Right 'a'

*Main> parseWithEof (satisfy (`elem` "abc")) "d"
Left (line 1, column 1):
unexpected "d"

*Main> parseWithEof (satisfy isDigit) "d"
Left (line 1, column 1):
unexpected "d"

*Main> parseWithEof (satisfy isDigit) "1"
Right '1'
```

You can see that it is easy to use `==`, or `elem` or one of the
functions from the Data.Char module.

If you look at the docs on hackage
<http://hackage.haskell.org/package/parsec-3.1.3/docs/Text-Parsec-Char.html>,
you can view the source. The implementations of most of the functions
in `Text.Parsec.Char` are straightforward. I recommend you look at the
source for all of these functions.

You can see in the source that the `satisfy` function is a little more
primitive than the other functions.

Here is the parser we used above in the `anyChar` parser:

```
anyChar :: Parser Char
```

If you look at the source via the haddock link above, you can see it
uses `satisfy`.

Here are some other simple wrappers of `satisfy` from
`Text.Parsec.Char` which use different validation functions.

The `char` parser parses a specific character which you supply:

```
char :: Char -> Parser Char
```

```
*Main> regularParse (char 'a') "a"
Right 'a'

*Main> regularParse (char 'a') "b"
Left (line 1, column 1):
unexpected "b"
expecting "a"
```

These parsers all parse single hardcoded characters

```
space :: Parser Char
newline :: Parser Char
tab :: Parser Char
```

They all return a `Char`. You might be able to guess what `Char` each
of them returns, you can double check your intuition using ghci.

These parser all parse one character from a hardcoded set of
characters:

```
upper :: Parser Char
lower :: Parser Char
alphaNum :: Parser Char
letter :: Parser Char
digit :: Parser Char
hexDigit :: Parser Char
octDigit :: Parser Char
```

In these cases, the return value is less redundant.

`oneOf` and `noneOf` parse any of the characters in the given list

```
oneOf :: [Char] -> Parser Char
noneOf :: [Char] -> Parser Char
```

These are just simple wrappers of satisfy using `elem`.


You should try all these parsers out in ghci, e.g.:

```

regularParse space " "

regularParse upper "A"

regularParse (char 'b') "B"

regularParse (oneOf "abc") "c"

```

Here are the final functions in `Text.Parsec.Char`:

`string` matches a complete string, one character at a time. I think
the implementation of this function is like it is for efficiency when
parsing from, e.g., `Data.Text.Text`, instead of `String`, but I'm not
sure. We will skip the detailed explanation of the implementation for
now.

```
string :: String -> Parser String
```

```
*Main> regularParse (string "one") "one"
Right "one"

*Main> regularParse (string "one") "two"
Left (line 1, column 1):
unexpected "t"
expecting "one"
```

Here is the `spaces` parser, which, if you look at the source, you can
see uses a combinator (`skipMany`). We will cover this combinator
shortly.

```
spaces :: Parser ()
```

```
*Main> regularParse spaces ""
Right ()

*Main> regularParse spaces " "
Right ()

*Main> regularParse spaces "   "
Right ()

*Main> regularParse spaces " a  "
Right ()

*Main> regularParse spaces "a a  "
Right ()
```

It always succeeds.

== A couple of helper executables

Here are two exes which you can use to parse either a string or a file
to help you experiment. This will save you having to figure out how to
write this boilerplate until later.

<https://github.com/JakeWheat/intro_to_parsing/blob/master/ParseFile.lhs>

<https://github.com/JakeWheat/intro_to_parsing/blob/master/ParseString.lhs>

Now you can easily experiment using ghci, or with a string on the
command line, or by putting the text to parse into a file and parsing
that.

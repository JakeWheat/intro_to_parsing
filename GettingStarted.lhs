
This is an introduction to parsing with Haskell and Parsec.

Prerequisites: you should know some basic Haskell and have the Haskell
Platform installed (or GHC and Parsec installed in some other way).

This tutorial was written using GHC 7.6.3 and Parsec 3.1.3, which are
the versions which come with the Haskell Platform 2013.2.0.0.

This file is a Literate Haskell file, available here:
<https://github.com/JakeWheat/intro_to_parsing/blob/master/GettingStarted.lhs>

All the other files in this tutorial collection are Literate Haskell
files, and they are all available here:
<https://github.com/JakeWheat/intro_to_parsing/>

I recommend you download them all, and follow along in your favourite
editor, and use ghci to experiment. To download, use:

```
git clone https://github.com/JakeWheat/intro_to_parsing.git
```

Here are the imports.

> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Char (anyChar)
> import Text.Parsec.String.Char
> import Boilerplate (regularParse, parseWithEof, parseWithLeftOver)
> import Data.Char

= Getting started

The first parser:

```
anyChar :: Parser Char
```

This parser is in the module Text.Parsec.Char. There is a wrapper in
this tutorial's project, Text.Parsec.String.Char, which gives this
function a simplified type.

Whenever we write a parser which parses to a value of type a, we give
it the return type of `Parser a`. In this case, we parse a character
so the return type is `Parser Char`. The `Parser` type itself is in
the module Text.Parsec.String. We will cover this in more detail
later.

Let's use this parser. Change to the directory where you saved this
.lhs file, and run ghci. Then type in ':l GettingStarted.lhs'. You
can run the parser using a wrapper, enter the following at the ghci
prompt: 'regularParse anyChar "a"'.

```
regularParse :: Parser a -> String -> Either ParseError a
```

Here is a transcript:

```
$ ghci
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :l GettingStarted.lhs
[1 of 1] Compiling Main             ( GettingStarted.lhs, interpreted )
Ok, modules loaded: Main.
*Main> regularParse anyChar "a"
Right 'a'
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
regularParse will come later.

Here are two alternatives to regularParse you can also use for
experimenting for the time being:

```
parseWithEof :: Parser a -> String -> Either ParseError a
```

```
parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
```

These can be useful when you are not sure if your parser is consuming
all your input string or not. The eof parser can tell you the answer
to this, and the leftover parser can additionally tell you what was
left over.

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

= Type signatures

The real Parsec functions have quite complex type signatures. This
makes a lot of things very tricky before you understand them, and
makes some of the error messages you'll see really difficult to
understand. I've created some wrapper modules, which set the types of
all the functions from Parsec we use to be much more restricted. This
will make the types easy to understand, and reduce the amount of
tricky to understand compiler errors you get. You can use this
approach when writing your own parser code with Parsec. These wrapper
modules are created with the following name pattern: Text.Parsec.Char
-> Text.Parsec.String.Char.

Later on, we will look at the general types in more detail.

= Text.Parsec.Char

Let's go through some of the functions in Text.Parsec.Char module from
the Parsec package. The haddock is avaialble here:
<http://hackage.haskell.org/package/parsec-3.1.3/docs/Text-Parsec-Char.html>.

Here is the `satisfy` function, with its full type signature.

```
satisfy :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Char
```

This is one of the main primitive functions in Parsec. This looks at
the next character from the current input, and if the function (Char
-> Bool) returns true for this character, it 'pops' it from the input
and returns it. In this way, the current position in the input string
is tracked behind the scenes.

In the simplified type wrappers, the satisfy function's type is this:

```
satisfy :: (Char -> Bool) -> Parser Char
```

This makes it a bit clearer what it is doing. All the functions in
Text.Parsec.Char are reproduced in the local Text.Parsec.String.Char
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
you can view the source. Most of the functions in Text.Parsec.Char are
straightforward. I recommend you look at the source for all of these
functions.

You can see in the source that the satisfy function is a little more
primitive than the other functions. I'm going to skip the explanation
for the implementation of satisfy for now.

Here is the parser we used above in the anyChar parser:

```
anyChar :: Parser Char
```

If you look at the source via the haddock link above, you can see it
uses satisfy.

Here are some other simple wrappers of satisfy from Text.Parsec.Char
which use different validation functions.

The char parses a specific character which you supply:

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

They all return a Char. You might be able to guess what Char each of
them returns, you can double check your intuition using ghci.

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

oneOf and noneOf parse any of the characters in the given list

```
oneOf :: [Char] -> Parser Char
noneOf :: [Char] -> Parser Char
```

These are just simple wrappers of satisfy using elem.


You should try all these parsers out in GHCI, e.g.:

```

regularParse space " "

regularParse upper "A"

regularParse (char 'b') "B"

regularParse (oneOf "abc") "c"

```

Here are the final functions in Text.Parsec.Char:

string matches a complete string, one character at a time. I think the
implementation of this function is like it is for efficiency when
parsing from, e.g., Data.Text.Text, instead of String, but I'm not
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
see uses a combinator (skipMany). We will cover this combinator
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

= A couple of helper executables

Here are two exes which you can use to parse either a string or a file
to help you experiment. This will save you having to figure out how to
write this boilerplate right now.

<https://github.com/JakeWheat/intro_to_parsing/blob/master/ParseFile.lhs>

<https://github.com/JakeWheat/intro_to_parsing/blob/master/ParseString.lhs>

Now you can easily experiment using ghci, or with a string on the
command line, or by putting the text to parse into a file and parsing
that.

TODO: transcript of using these exes.

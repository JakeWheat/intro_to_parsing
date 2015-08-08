
This is the example exe file which will parse a string given on the
command line. You can substitute your own parser for the parser given
below.


> import System.Environment
> import Text.Parsec
> import Text.Parsec.String
> import Control.Monad

> main :: IO ()
> main = do
>     a <- getArgs
>     case a of
>       [str] -> either print print $ parse myParser "" str
>       _ -> error "please pass one argument with the string to parse"

This is the parser which you can replace with your own code:

> myParser :: Parser ()
> myParser = void $ string "correct"

Here is an example of running this program:

```
$ runhaskell ParseString.lhs x
(line 1, column 1):
unexpected "x"
expecting "correct"

$ runhaskell ParseString.lhs correct
()
```

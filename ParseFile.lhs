
This is the example exe file which will parse a string from the file
given on the command line. You can substitute your own parser for the
parser given below.


> import System.Environment
> import Text.Parsec
> import Text.Parsec.String as P
> import Control.Monad

> main :: IO ()
> main = do
>     a <- getArgs
>     case a of
>       [str] -> parseFromFile myParser str >>= either print print
>       _ -> error "please pass one argument with the file containing the text to parse"

This is the parser which you can replace with your own code:

> myParser :: P.Parser ()
> myParser = void $ string "correct"

Here is an example of running this program:

$ echo x > source_text && runhaskell ParseFile.lhs source_text
"source_text" (line 1, column 1):
unexpected "x"
expecting "correct"
$ echo correct > source_text && runhaskell ParseFile.lhs source_text
()


This is an example exe which parses a list of SQL queries in a file
separated by semi colons.

> import System.Environment
> import Text.Parsec (ParseError, errorPos, sourceName, sourceLine, sourceColumn)
> import Text.Parsec.String
> import Text.Parsec.String.Char
> import Text.Parsec.String.Combinator
> import Control.Applicative ((<$>),(<*), (*>),(<*>), (<$), (<|>), many)
> import Control.Monad
> import SimpleSQLQueryParser0
> import Data.List
> import qualified PrettyPrinting0 as P

> main :: IO ()
> main = do
>     a <- getArgs
>     case a of
>       [str] -> parseFromFile myParser str
>                >>= either (putStrLn . showError)
>                           (putStrLn . intercalate "\n" . map P.prettyQueryExpr)
>       _ -> error "please pass one argument with the file containing the queries to parse"

> myParser :: Parser [QueryExpr]
> myParser = whitespace
>            *> sepBy1 queryExpr semi
>            <* optional semi
>            <* eof
>   where semi = void $ lexeme $ char ';'

> showError :: ParseError -> String
> showError e =
>     let p = errorPos e
>     in sourceName p ++ ":" ++ show (sourceLine p) ++ ":"
>        ++ show (sourceColumn p) ++ ":\n"
>        ++ show e

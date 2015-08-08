
= Overview

This is a tutorial about an issue with the token parsing we have so
far.

> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Parsec (try)
> import Text.Parsec.String.Combinator (many1, notFollowedBy)
> import Text.Parsec.String.Char (digit, string, oneOf, satisfy, char, letter)
>
> import Control.Applicative ((<$>), (<*>), (<*), many, (<$), (<|>))
> import Control.Monad (void, guard)
>
> import qualified Text.Parsec.String.Expr as E
> import FunctionsAndTypesForParsing

Here is a simplified expression type and parser:

> data SimpleExpr = Num Integer
>                 | BinaryOp SimpleExpr String SimpleExpr
>                    deriving (Eq,Show)
>
> simpleExpr :: Parser SimpleExpr
> simpleExpr = E.buildExpressionParser table num
>
> table :: [[E.Operator SimpleExpr]]
> table = [[binary "<=" E.AssocNone
>          ,binary ">=" E.AssocNone]
>         ,[binary "<" E.AssocNone
>          ,binary ">" E.AssocNone]
>         ]
>   where
>     binary name assoc =
>         E.Infix (mkBinOp name <$ symbol name) assoc
>     mkBinOp nm a b = BinaryOp a nm b
>
> num :: Parser SimpleExpr
> num = Num <$> integer
>
> whitespace :: Parser ()
> whitespace = void $ many $ oneOf " \n\t"
>
> lexeme :: Parser a -> Parser a
> lexeme p = p <* whitespace
>
> integer :: Parser Integer
> integer = read <$> lexeme (many1 digit)
>
> symbol :: String -> Parser String
> symbol s = lexeme $ string s

Let's try it out:

```
*Main> regularParse simpleExpr "1=2"
Right (Num 1)

*Main> regularParse simpleExpr "1>=2"
Right (BinaryOp (Num 1) ">=" (Num 2))

*Main> regularParse simpleExpr "1>2"
Left (line 1, column 2):
unexpected "2"
expecting ">="
```

What happened? The parser tried to parse > as >=, failed, and since
the failure consumed some input (the first >), it failed completely.

We are going to change the symbol parser to fix this. Here is a
parameterized version of the simpleExpr parser so we can try a few
variations out.

> simpleExprP :: (String -> Parser String) -> Parser SimpleExpr
> simpleExprP sym = E.buildExpressionParser (tableP sym) num
>
> tableP :: (String -> Parser String) -> [[E.Operator SimpleExpr]]
> tableP sym = [[binary "<=" E.AssocNone
>               ,binary ">=" E.AssocNone]
>              ,[binary "<" E.AssocNone
>               ,binary ">" E.AssocNone]]
>   where
>     binary name assoc =
>         E.Infix (mkBinOp name <$ sym name) assoc
>     mkBinOp nm a b = BinaryOp a nm b

Let's reproduce the failure:

```
*Main> regularParse (simpleExprP symbol) "1>=2"
Right (BinaryOp (Num 1) ">=" (Num 2))

*Main> regularParse (simpleExprP symbol) "1>2"
Left (line 1, column 2):
unexpected "2"
expecting ">="
```

We are going to look at two possible solutions.

1. Let's use `try`:

```
*Main> regularParse (simpleExprP (try . symbol)) "1>=2"
Right (BinaryOp (Num 1) ">=" (Num 2))

*Main> regularParse (simpleExprP (try . symbol)) "1>2"
Right (BinaryOp (Num 1) ">" (Num 2))
```

This seems to have done the job. There is still a problem
though. Consider a case when the precedence is the other way round -
the `<` and `>` are higher precedence than `<=` and `>=`,

> simpleExprP1 :: (String -> Parser String) -> Parser SimpleExpr
> simpleExprP1 sym = E.buildExpressionParser (tableP1 sym) num
>
> tableP1 :: (String -> Parser String) -> [[E.Operator SimpleExpr]]
> tableP1 sym = [[binary "<" E.AssocNone
>               ,binary ">" E.AssocNone]
>              ,[binary "<=" E.AssocNone
>               ,binary ">=" E.AssocNone]]
>   where
>     binary name assoc =
>         E.Infix (mkBinOp name <$ sym name) assoc
>     mkBinOp nm a b = BinaryOp a nm b

```
*Main> regularParse (simpleExprP1 (try . symbol)) "1>2"
Right (BinaryOp (Num 1) ">" (Num 2))

*Main> regularParse (simpleExprP1 (try . symbol)) "1>=2"
Left (line 1, column 3):
unexpected "="
expecting digit
```

Although the precendence order is a little contrived in this case,
this issue could easily crop up for real when we start adding more
operators. Let's fix it now.

This could be solved by adding a `try` at a earlier place in the
parsing. Because of how the `buildExpressionParser` function works,
it's not obvious where the `try` could go.

Let's try tackling the problem in a different way. One way of looking
at this is to consider that the symbol parser stops parsing too soon:

```
*Main> parseWithLeftOver (symbol ">") ">="
Right (">","=")
```

What it should do is keep parsing symbol characters until it gets a
result string which can't be a symbol, and stop one character before
this..

Here is a slightly naive way of doing it, which will be good enough
for quite a while:

> symbol1 :: String -> Parser String
> symbol1 s = try $ lexeme $ do
>     u <- many1 (oneOf "<>=+-^%/*")
>     guard (s == u)
>     return s

Here is a similar alternative:

> symbol2 :: String -> Parser String
> symbol2 s = try $ lexeme $ do
>     void $ string s
>     notFollowedBy (oneOf "<>=+-^%/*")
>     return s

Let's try them out:

```
*Main> parseWithLeftOver (symbol1 ">") ">="
Left (line 1, column 3):
unexpected end of input

*Main> parseWithLeftOver (symbol1 ">") ">"
Right (">","")

*Main> parseWithLeftOver (symbol1 ">") ">= 3"
Left (line 1, column 3):
unexpected " "

*Main> parseWithLeftOver (symbol1 ">=") ">= 3"
Right (">="," 3")


```
The error messages don't seem very good, but it parses and fails to
parse correctly.

```
*Main> parseWithLeftOver (symbol2 ">") ">="
Left (line 1, column 3):
unexpected '='

*Main> parseWithLeftOver (symbol2 ">") ">"
Right (">","")

*Main> parseWithLeftOver (symbol2 ">") ">= 3"
Left (line 1, column 3):
unexpected '='

*Main> parseWithLeftOver (symbol2 ">=") ">= 3"
Right (">="," 3")
```

This one appears to give better error messages in this limited
scenario, apart from that they both work the same.

Let's try them out in the full expression parser:

```
*Main> regularParse (simpleExprP symbol1) "1>=2"
Right (BinaryOp (Num 1) ">=" (Num 2))

*Main> regularParse (simpleExprP symbol1) "1>2"
Right (BinaryOp (Num 1) ">" (Num 2))

*Main> regularParse (simpleExprP symbol2) "1>=2"
Right (BinaryOp (Num 1) ">=" (Num 2))

*Main> regularParse (simpleExprP symbol2) "1>2"
Right (BinaryOp (Num 1) ">" (Num 2))
```

They both work fine here. Let's see some error messages in this
context.

```
*Main> parseWithEof (simpleExprP symbol1) "1>*2"
Left (line 1, column 4):
unexpected "2"
expecting operator

*Main> parseWithEof (simpleExprP symbol2) "1>*2"
Left (line 1, column 4):
unexpected '*'
expecting operator
```

Both error messages are a bit crap. So much for the second variation
producing better error messages.

Let's look at the equivalent issue with respect to keyword parsing. We
can get a similar problem here.

> keyword :: String -> Parser String
> keyword s = try $ string s

```
*Main> parseWithEof (keyword "not") "not"
Right "not"

*Main> parseWithEof (keyword "not") "nothing"
Left (line 1, column 4):
unexpected 'h'
expecting end of input

*Main> parseWithEof (keyword "not" <|> keyword "nothing") "nothing"
Left (line 1, column 4):
unexpected 'h'
expecting end of input

*Main> parseWithEof (keyword "nothing" <|> keyword "not") "nothing"
Right "nothing"
```

We can fix this overlapping prefix issue by reordering the
choices. But let's fix the `keyword` parser in a similar way to the
symbol parser.

TODO: I don't know if symbol is the right name, I don't think Parsec
usually uses symbol in this way. Maybe it should be called operator.

> identifier :: Parser String
> identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

> keyword1 :: String -> Parser String
> keyword1 k = try $ do
>     i <- identifier
>     guard (i == k)
>     return k

TODO: later note in error messages about choosing identifier here
instead of e.g. many1 letter.

```
*Main> parseWithEof (keyword1 "not") "not"
Right "not"

*Main> parseWithEof (keyword1 "not") "nothing"
Left (line 1, column 8):
unexpected end of input
expecting digit, letter or "_"

*Main> parseWithEof (keyword1 "not" <|> keyword1 "nothing") "nothing"
Right "nothing"

*Main> parseWithEof (keyword1 "nothing" <|> keyword1 "not") "nothing"
Right "nothing"

*Main> parseWithEof (keyword1 "not" <|> keyword1 "nothing") "not"
Right "not"

*Main> parseWithEof (keyword1 "nothing" <|> keyword1 "not") "not"
Right "not"
```

Try implementing the `keyword2` parser which uses `notFollowedBy`
instead of `guard`, using something analogous to the change from
`symbol1` to `symbol2` above.

After this, you can try reimplementing the expression parser from the
Text.Parsec.Expr tutorial using the new symbol and keyword parsers.

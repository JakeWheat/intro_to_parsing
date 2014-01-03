
Text.Parsec.Expr allows building expression parsers with a range of
operators with different precedences and associativities easily.

Text.Parsec.Expr can be great to quickly get a prototype parser up and
running, but sometimes the lack of control means you have to replace
it with something more flexible later on.

> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Combinator (many1, between)
> import Text.Parsec.String.Char (letter, char, digit, string, oneOf)
>
> import Control.Applicative ((<$>), (<*>), (<*), (<|>), many, (<$))
> import Control.Monad (void)
>
> import qualified Text.Parsec.String.Expr as E
> import Boilerplate

The functionality will be enough for the simple SQL query parser later
in the tutorial, but for a more complete SQL parser it will fall short
(especially when parsing some non-standard dialects).

Let's extend the SimpleExpression type and parsers to cover a range of
operators with different precedences and associativity.

= expressions with plus and times

Let's start with a simple case: + and * with the usual fixity. Here is
the abstract syntax:

> data PlusTimesExpr = PteVar String
>                    | PteNum Integer
>                    | PteParens PlusTimesExpr
>                    | Plus PlusTimesExpr PlusTimesExpr
>                    | Times PlusTimesExpr PlusTimesExpr
>                          deriving (Eq,Show)

> plusTimesExpr :: Parser PlusTimesExpr
> plusTimesExpr = E.buildExpressionParser pteTable pteTerm

> pteTable :: [[E.Operator PlusTimesExpr]]
> pteTable = [[E.Infix (Times <$ symbol "*") E.AssocLeft]
>            ,[E.Infix (Plus <$ symbol "+") E.AssocLeft]
>            ]

Here you can see the operator parsers are the same as the previous
`SimpleExpr` parser which used `chainl1`: `Times <$ symbol "*"` and
`Plus <$ symbol "+"`. We just wrapped these up in the `E.Infix`
constructor with the associativity, and put them in a list of lists
which represents the precendence classes.

Here is the term parser and components. All this is just the same as
the `SimpleExpr` parser a previous tutorial.

> pteTerm :: Parser PlusTimesExpr
> pteTerm = pteVar <|> pteNum <|> pteParens

> pteNum :: Parser PlusTimesExpr
> pteNum = PteNum <$> integer

> pteVar :: Parser PlusTimesExpr
> pteVar = PteVar <$> identifier

> pteParens :: Parser PlusTimesExpr
> pteParens = PteParens <$> between (symbol "(") (symbol ")") plusTimesExpr

support functions:

> whitespace :: Parser ()
> whitespace = void $ many $ oneOf " \n\t"

> lexeme :: Parser a -> Parser a
> lexeme p = p <* whitespace

> integer :: Parser Integer
> integer = read <$> lexeme (many1 digit)

> identifier :: Parser String
> identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

> symbol :: String -> Parser String
> symbol s = lexeme $ string s

Here you can see the precendence in action:

```
*Main> regularParse plusTimesExpr "a + b * c"
Right (Plus (PteVar "a") (Times (PteVar "b") (PteVar "c")))

*Main> regularParse plusTimesExpr "a * b + c"
Right (Plus (Times (PteVar "a") (PteVar "b")) (PteVar "c"))
```

= a full featured expression type

Now let's try a much bigger example with lots more operators. Now we
are thinking ahead to the first version of the SQL query parser, and
preparing for this.

Here are our new operators in precedence order:

== unary + -

```
+a
-3
```

== exponentiation

```
a ^ 3
```

associativity: left

== multiplication, division, modulo

```
a * 3
3 / b
a % 5
```

associativity: left

== addition, subtraction

```
a + b
a - b
```

associativity: left

== less than, greater than

```
a < b
a > b
```

associativity: none

== equals

```
a = 3
```

associativity: right

== not

```
not a
```

== and

```
a and b
```

associativity: left

== or

```
a or b
```

associativity: left

Here is the abstract syntax type:

> data SimpleExpr = Num Integer
>                 | Var String
>                 | Parens SimpleExpr
>                 | PrefixOp String SimpleExpr
>                 | BinaryOp SimpleExpr String SimpleExpr
>                    deriving (Eq,Show)

Here is the new expression parser:

> simpleExpr :: Parser SimpleExpr
> simpleExpr = E.buildExpressionParser table term

> table :: [[E.Operator SimpleExpr]]
> table = [[prefix "-", prefix "+"]
>         ,[binary "^" E.AssocLeft]
>         ,[binary "*" E.AssocLeft
>          ,binary "/" E.AssocLeft
>          ,binary "%" E.AssocLeft]
>         ,[binary "+" E.AssocLeft
>          ,binary "-" E.AssocLeft]
>         ,[binary "<" E.AssocNone
>          ,binary ">" E.AssocNone]
>         ,[binary "=" E.AssocRight]
>         ,[prefix "not"]
>         ,[binary "and" E.AssocLeft]
>         ,[binary "or" E.AssocLeft]
>         ]
>   where
>     binary name assoc =
>         E.Infix (mkBinOp name <$ symbol name) assoc
>     mkBinOp nm a b = BinaryOp a nm b
>     prefix name = E.Prefix (PrefixOp name <$ symbol name)

TODO: expand and explain the bits.

Here is the term parser.

> term :: Parser SimpleExpr
> term = var <|> num <|> parens

> num :: Parser SimpleExpr
> num = Num <$> integer

> var :: Parser SimpleExpr
> var = Var <$> identifier

> parens :: Parser SimpleExpr
> parens = between (symbol "(") (symbol ")") simpleExpr

TODO: write lots of parsing examples, including parse failures with
ambiguity.

issue: double prefix op.

issues: notx, <= / <

The source in Text.Parsec.Expr is not too big. You can have a look and
try to understand it. There are several standard approaches in parsing
theory to parse expressions with data driven precendences and
associativity. I don't know which one Text.Parsec.Expr uses, but if
you find these and read about them, the implementation might be a bit
more understandable.

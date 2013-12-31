
Here are some of the more powerful tools which come with Parsec:

Text.Parsec.Expr

This allows building expression parsers with a range of operators with
different precedences and associativities easily.

Text.Parsec.Perm

This can parse things in an arbitrary order, including optional parts.

Text.Parsec.Token

This can be used quickly create a set of token parsers handling lots
of little issues which you otherwise have to deal with manually.

Text.Parsec.Expr and Text.Parsec.Token can be great to quickly get a
prototype parser up and running, but sometimes the lack of control
means you have to replace it with something more flexible later on.


> --import qualified Text.Parsec as P
> import Text.Parsec (oneOf, many1, letter, char, digit, string)
> import qualified Text.Parsec.String as P (Parser)
> import Control.Applicative ((<$>), (<*>), (<*), (<|>), many)
> import Control.Monad (void)
> import Control.Monad.Identity (Identity)
> import qualified Text.Parsec.Expr as E


= Text.Parsec.Expr

This can be used to build an expression parser with operators with
different associativities and precedences easily. This functionality
will be enough for the simple SQL query parser later in the tutorial,
but for a more complete SQL parser it will fall short (especially when
parsing some non-standard dialects).

TODO: just show some examples

Let's extend the SimpleExpression type and parsers to cover a range of
operators with different precedences and associativity.

Here are our new operators in precedence order:

== unary + -

~~~~
+a
-3
~~~~

== exponentiation

~~~~
a ^ 3
~~~~

associativity: left

== multiplication, division, modulo

~~~~
a * 3
3 / b
a % 5
~~~~

associativity: left

== addition, subtraction

~~~~
a + b
a - b
~~~~

associativity: left

== less than, greater than

~~~~
a < b
a > b
~~~~

associativity: none

== equals

~~~~
a = 3
~~~~

associativity: right

== not

~~~~
not a
~~~~

== and

~~~~
a and b
~~~~

associativity: left

== or

~~~~
a or b
~~~~

associativity: left

Here is the abstract syntax type:

> data SimpleExpr2 = Num2 Integer
>                  | Var2 String
>                  | PrefixOp2 String SimpleExpr2
>                  | BinaryOp2 SimpleExpr2 String SimpleExpr2
>                    deriving (Eq,Show)

Here is the new expression parser:

> simpleExpr2 :: P.Parser SimpleExpr2
> simpleExpr2 = E.buildExpressionParser table term

> term :: P.Parser SimpleExpr2
> term = var2 <|> num2

> table :: [[E.Operator String () Identity SimpleExpr2]]
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
>     binary name assoc = E.Infix (void (opName name)
>                                  >> return (\a b -> BinaryOp2 a name b))
>                                 assoc
>     prefix name = E.Prefix (void (opName name)
>                             >> return (PrefixOp2 name))
>     opName s = string s <* whiteSpace

TODO: expand and explain the bits.

> num2 :: P.Parser SimpleExpr2
> num2 = Num2 <$> integer

> var2 :: P.Parser SimpleExpr2
> var2 = Var2 <$> identifier

TODO: write lots of parsing examples, including parse failures with ambiguity.

issue: double prefix op.

= Text.Parsec.Perm

This can parse a bunch of different things in any order. TODO:
examples.

= Text.Parsec.Token

This code contains helpers you can use to construct the 'token'
parsers instead of writing them by hand.

TODO: examples


> whiteSpace :: P.Parser ()
> whiteSpace = void $ many $ oneOf " \n\t"

> integer :: P.Parser Integer
> integer = read <$> many1 digit <* whiteSpace

> identifier :: P.Parser String
> identifier = (:) <$> firstChar <*> many nonFirstChar <* whiteSpace
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

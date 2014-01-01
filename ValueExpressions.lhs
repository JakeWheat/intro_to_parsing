
In this tutorial, we will build a parser for a subset of SQL value
expressions. These are roughly the same as the expressions used in
languages like Haskell or C. This will be something like an extension
to the simple expressions used in the last tutorial.

> {-# LANGUAGE TupleSections #-}
> module ValueExpressions where

> import Text.Groom (groom)
> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Char (oneOf, digit, string, anyChar, char, letter, alphaNum)
> import Text.Parsec.String.Combinator (many1, manyTill, eof, choice, between, sepBy, optionMaybe)
> import Text.Parsec.String.Parsec(try,parse)

> import Control.Applicative (many, (<*),(<$>), (*>), (<|>),(<$),(<*>))
> import Control.Monad (void,guard)
> --import Debug.Trace
> import Data.List (intercalate)
> import qualified Text.Parsec.String.Expr as E

Our value expressions will support literals, identifiers, asterisk,
some simple operators, case expression and parentheses. Here is the
lineup in more detail.

== Literals

It will just support integral and string literals at this time. Proper
SQL supports more literal types including some quite weird syntax
which we will skip for now.

~~~~
1
500
'string literal'
~~~~

== identifiers

We will use simple identifiers: an identifier may start with a letter
or underscore, and contain letters, underscores and numbers. Full SQL
identifiers are more complicated to support so we will skip this for
now also.

~~~~
a
something
_test_
a123
~~~~

== 'dotted identifiers'

We will do some limited support for identifiers with two parts
separated by a dot. I don't want to get into the exact meaning or the
various names used to describe these since it is a bit confusing,
especially in SQL. Both parts must parse according to the identifier
rules above.

~~~~
t.a
something.something_else
~~~~

== star

We will support the star as special expression which can be used at
the top level of select lists (and a few other places in SQL). We will
also support a 'dotted star'.

~~~~
*
t.*
~~~~

== function application

This represents a function application which syntactically looks like
the normal function application used in languages like C. The function
name must parse as a valid identifier according to the rules above.

~~~~
f()
g(1)
h(2,'something')
~~~~

== operators

We will only support a small range of binary operators for now plus a
single prefix unary operator (not). We will attempt to support correct
precedence and associativity for these. Here is a complete list of all
the supported operators.

~~~~
a = b
a > b
a < b
a >= b
a <= b
a != b
a <> b (two spellings of the same thing, we will parse them as
        separate operators though)
a and b
a or b
1 + 2
1 - 2
1 * 2
1 / 2
'some' || 'thing'
a like b
not a
~~~~

== case expression

There are two standard variations of case expressions in SQL. One is
more like a switch statement in C (but is an expression, not a
statement):

~~~~
case a
when 3 then 'got three'
when 5 then 'got five'
else 'neither'
end
~~~~

The other has a boolean expression in each branch:

~~~~
case
when a = 3 then 'a is three'
when b = 4 then 'b is four'
else 'neither'
end
~~~~

The else branch is optional.

== parentheses

It will parse and represent parentheses explicitly in the abstract
syntax.

~~~~
(1 + 2) * 3
~~~~

Here is a syntax type to represent value expressions.

> data ValueExpr = StringLiteral String
>                | NumberLiteral Integer
>                | Identifier String
>                | DIdentifier String String -- a.b
>                | Star
>                | DStar String -- t.*
>                | App String [ValueExpr]
>                | PrefixOp String ValueExpr
>                | BinaryOp ValueExpr String ValueExpr
>                | Case (Maybe ValueExpr) -- test value
>                       [(ValueExpr,ValueExpr)] -- when branches
>                       (Maybe ValueExpr) -- else value
>                | Parens ValueExpr
>                  deriving (Eq,Show)

Here is the explanation, some examples, test cases, and some simple
parsers for each of these variants.

== comments

Here is the whitespace parser which skips comments also

> whiteSpace :: Parser ()
> whiteSpace =
>     choice [simpleWhiteSpace *> whiteSpace
>            ,lineComment *> whiteSpace
>            ,blockComment *> whiteSpace
>            ,return ()]
>   where
>     lineComment = try (string "--")
>                   *> manyTill anyChar (void (char '\n') <|> eof)
>     blockComment = -- no nesting of block comments in SQL
>                    try (string "/*")
>                    -- TODO: why is try used here
>                    *> manyTill anyChar (try $ string "*/")
>     -- use many1 so we can more easily avoid non terminating loops
>     simpleWhiteSpace = void $ many1 (oneOf " \t\n")

TODO: lots more explanation

== Literal

Here are some examples so we can build up a test suite as we go.

> parseLiteralsTestData :: [(String,ValueExpr)]
> parseLiteralsTestData =
>     [("1", NumberLiteral 1)
>     ,("54321", NumberLiteral 54321)
>     ,("''", StringLiteral "")
>     ,("'test'", StringLiteral "test")]

It doesn't support non integral number literals.

We already saw how to write these parsers:

> integer :: Parser Integer
> integer = read <$> many1 digit <* whiteSpace

> integerLiteral :: Parser ValueExpr
> integerLiteral = NumberLiteral <$> integer

String literals:

> stringLiteral :: Parser ValueExpr
> stringLiteral = StringLiteral <$> (symbol_ "'" *> manyTill anyChar (symbol_ "'"))

Here is the symbol parser. I've created a wrapper which uses void
which can be used to avoid writing void in lots of places.

> symbol :: String -> Parser String
> symbol s = string s <* whiteSpace

> symbol_ :: String -> Parser ()
> symbol_ s = void $ symbol s

TODO: suffix issues with symbol parser

Here is the parser which can parse either kind of literal:

> literal :: Parser ValueExpr
> literal = integerLiteral <|> stringLiteral

Here is a small helper function to check the examples above. I've put
the test data and the parser as parameters so we can reuse it later.

> checkParse :: (Eq a, Show a) => Parser a -> [(String,a)] -> IO ()
> checkParse parser testData = do
>     let -- create a wrapper function which uses the parser function
>         parseit = parse (whiteSpace *> parser <* eof) ""
>         -- parse all the input strings
>         parsed = map (parseit . fst) testData
>         triples = zip testData parsed
>         -- this function checks if a parse produced the expected
>         -- result
>         checkit ((_txt,expected),got) =
>             either (const True) (/=expected) got
>         -- keep all the failed parses
>         failed = filter checkit triples
>         -- format these results
>         report ((txt,expected),got) = "parsing " ++ txt
>                                       ++ " failed, expected\n"
>                                       ++ groom expected
>                                       ++ "\ngot\n" ++ groom got
>     putStrLn $ intercalate "\n" $ map report failed

You can use

~~~~
checkParse literal parseLiteralsTestData
~~~~

at the ghci prompt to check the literal examples with the literal
parser.

I like to change the tests briefly every so often to break one of
them, then run the tests to check that the failure is detected (then
change it back of course):

~~~~
*SqlParser> checkParse literal parseLiteralsTestData
parsing 'test' failed, expected
Literal "test1"
got
Right (Literal "test")
~~~~

This is a quick sanity check to make sure the tests will actually
report failure.

== identifier

Some examples:

> parseIdentifierTestData :: [(String,ValueExpr)]
> parseIdentifierTestData =
>     [("_", Identifier "_")
>     ,("a", Identifier "a")
>     ,("a_1", Identifier "a_1")]


Here a parser for the identifier token

> identifierString' :: Parser String
> identifierString' =
>     (:) <$> letterOrUnderscore
>         <*> many letterDigitOrUnderscore <* whiteSpace
>   where
>     letterOrUnderscore = letter <|> char '_'
>     letterDigitOrUnderscore = digit <|> letterOrUnderscore

and a parser for identifier expressions

> identifier' :: Parser ValueExpr
> identifier' = Identifier <$> identifierString

We can check these parsers at the ghci prompt using 'checkParse
identifier' parseIdentifierTestData'.

There will be an issue with this parser which will be covered later.

== dotted identifier

> parseDottedIdentifierTestData :: [(String,ValueExpr)]
> parseDottedIdentifierTestData =
>     [("t.a", DIdentifier "t" "a")]

> dottedIdentifier :: Parser ValueExpr
> dottedIdentifier = DIdentifier <$> identifierString
>                                <*> (symbol_ "." *> identifierString)

== stars

> parseStarTestData :: [(String,ValueExpr)]
> parseStarTestData =
>     [("*", Star)
>     ,("t.*", DStar "t")]

> star :: Parser ValueExpr
> star = choice [Star <$ symbol_ "*"
>               ,DStar <$> (identifierString <* symbol_ "." <* symbol_ "*")]

== app

The App constructor is used for syntax which looks like regular
function application: f(), f(a), f(a,b), etc.

> parseAppTestData :: [(String,ValueExpr)]
> parseAppTestData = [("f()", App "f" [])
>                    ,("f(1)", App "f" [NumberLiteral 1])
>                    ,("f(1,a)", App "f" [NumberLiteral 1, Identifier "a"])]

The valueExpr parser will appear later.

> app :: Parser ValueExpr
> app = App <$> identifierString <*> parens (commaSep valueExpr)

There are two new helper parsers:

> parens :: Parser a -> Parser a
> parens = between (symbol_ "(") (symbol_ ")")

> commaSep :: Parser a -> Parser [a]
> commaSep = (`sepBy` symbol_ ",")

== case

Here are the examples/tests for case.

> parseCaseTestData :: [(String,ValueExpr)]
> parseCaseTestData =
>     [("case a when 1 then 2 end"
>      ,Case (Just $ Identifier "a") [(NumberLiteral 1,NumberLiteral 2)]
>            Nothing)
>     ,("case a when 1 then 2 when 3 then 4 end"
>      ,Case (Just $ Identifier "a") [(NumberLiteral 1, NumberLiteral 2)
>                                    ,(NumberLiteral 3, NumberLiteral 4)]
>            Nothing)
>     ,("case a when 1 then 2 when 3 then 4 else 5 end"
>      ,Case (Just $ Identifier "a") [(NumberLiteral 1, NumberLiteral 2)
>                                    ,(NumberLiteral 3, NumberLiteral 4)] (Just $ NumberLiteral 5))
>     ,("case when a=1 then 2 when a=3 then 4 else 5 end"
>      ,Case Nothing [(BinaryOp (Identifier "a") "=" (NumberLiteral 1), NumberLiteral 2)
>                    ,(BinaryOp (Identifier "a") "=" (NumberLiteral 3), NumberLiteral 4)]
>                    (Just $ NumberLiteral 5))
>     ]

Here is the parser:

> scase :: Parser ValueExpr
> scase =
>     Case <$> (try (keyword_ "case") *> optionMaybe (try valueExpr))
>          <*> many1 swhen
>          <*> optionMaybe (try (keyword_ "else") *> valueExpr)
>          <* keyword_ "end"
>   where
>     swhen = keyword_ "when" *>
>             ((,) <$> valueExpr <*> (keyword_ "then" *> valueExpr))

I've added a new helper parser 'keyword'. This parser suffers from the
same issue as the symbol parser regarding valid suffix characters. It
suffers from more issues since e.g. keyword 'select' will parse this
string 'selectx', which is even more wrong.

> keyword :: String -> Parser String
> keyword s = string s <* whiteSpace

> keyword_ :: String -> Parser ()
> keyword_ s = keyword s *> return ()

In fact, it's the same as the symbol parser. We can use this for now
just to document the code, and if we fix the keyword and symbol
parsers later to be more exact then we won't need to change the code
which uses them.

TODO: put in the identifier with blacklist here: the when issue.

> identifierString :: Parser String
> identifierString = do
>     s <- (:) <$> letterOrUnderscore
>              <*> many letterDigitOrUnderscore <* whiteSpace
>     guard (s `notElem` blacklist)
>     return s
>   where
>     letterOrUnderscore = letter <|> char '_'
>     letterDigitOrUnderscore = digit <|> letterOrUnderscore
>     blacklist = ["as", "from", "where", "having", "group", "order"
>                 ,"inner", "left", "right", "full", "natural", "join"
>                 ,"on", "using", "when", "then", "case", "end"]

TODO: talk about what must be in the blacklist, and what doesn't need
to be. This should be later.

> identifier :: Parser ValueExpr
> identifier = Identifier <$> identifierString

TODO: follow up on try, error messages.

= parens

> sparens :: Parser ValueExpr
> sparens = Parens <$> parens valueExpr

= operators

Here is our operator table. I followed the precedences given here:
http://www.postgresql.org/docs/9.3/static/sql-syntax-lexical.html#SQL-PRECEDENCE

> table :: [[E.Operator ValueExpr]]
> table = [[binary "*" E.AssocLeft
>          ,binary "/" E.AssocLeft]
>         ,[binary "+" E.AssocLeft
>          ,binary "-" E.AssocLeft]
>         ,[binary ">=" E.AssocNone
>          ,binary "<=" E.AssocNone
>          ,binary "!=" E.AssocRight
>          ,binary "<>" E.AssocRight
>          ,binary "||" E.AssocRight
>          ,binary "like" E.AssocNone]
>         ,[binary "<" E.AssocNone
>          ,binary ">" E.AssocNone]
>         ,[binary "=" E.AssocRight]
>         ,[prefix "not"]
>         ,[binary "and" E.AssocLeft]
>         ,[binary "or" E.AssocLeft]
>         ]
>   where
>     binary name assoc = E.Infix (void (opName name)
>                                  >> return (\a b -> BinaryOp a name b))
>                                 assoc
>     prefix name = E.Prefix (void (opName name)
>                             >> return (PrefixOp name))

TODO: talk about a parser which is just string: put this in the
expression parser tutorial?

>     opName s = try $ do
>         x <- many1 (alphaNum <|> oneOf "*/+-><=!|")
>         guard (x == s)
>         whiteSpace



Here is the value expr parser:

> valueExpr :: Parser ValueExpr
> valueExpr = E.buildExpressionParser table term
>   where
>     term = choice [literal
>                   ,scase
>                   ,try app
>                   ,try star
>                   ,try dottedIdentifier
>                   ,identifier
>                   ,sparens]

In the 'term' parser, I've used try and ordered the choices carefully
so that all the parsers work OK. I like to rearrange code like this to
left factor it, even though not having to left factor is one of the
advertised benefits of Parsec and similar libraries. However, I think
left factoring probably makes the parser code faster and I think can
give better error messages as well. (TODO: update when confirmed)

TODO: Here are some experiments to show the problems solved by details
and examples of it not working when try is removed or the order is
changed.

TODO: exercise: left factor to remove all the trys.


> parseOpTestData :: [(String,ValueExpr)]
> parseOpTestData =
>     map makeBinOpTest binOpNames
>     ++ [("not a", PrefixOp "not" (Identifier "a"))]
>   where
>     makeBinOpTest nm = ("a " ++ nm ++ " b"
>                        ,BinaryOp (Identifier "a") nm (Identifier "b"))

> binOpNames :: [String]
> binOpNames = ["=", "<=", ">="
>              ,"!=", "<>", "<", ">"
>              ,"and", "or"
>              ,"*", "/", "+", "-"
>              ,"||", "like"]

TODO: add sanity tests for fixities

> parseValueExprTestData :: [(String,ValueExpr)]
> parseValueExprTestData = concat [parseLiteralsTestData
>                                 ,parseIdentifierTestData
>                                 ,parseDottedIdentifierTestData
>                                 ,parseStarTestData
>                                 ,parseAppTestData
>                                 ,parseOpTestData
>                                 ,parseCaseTestData]

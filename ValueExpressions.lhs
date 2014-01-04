
In this tutorial, we will build a parser for a subset of SQL value
expressions. These are roughly the same as the expressions used in
languages like Haskell or C. This will follow on from the work on
expressions in previous tutorials.

Our value expressions will support literals, identifiers, asterisk,
some simple operators, case expression and parentheses.

The phrase 'value expression' is from the ANSI SQL standards. What we
will develop here isn't exactly ANSI SQL value expressions, and we
won't use them exactly how the standards do, but the differences
really aren't important right now. I will come back to this in a later
tutorial.

> {-# LANGUAGE TupleSections #-}
> module ValueExpressions (ValueExpr(..)
>                         ,valueExpr
>                         ,keyword
>                         ,symbol
>                         ,identifier
>                         ,makeTest
>                         ,blackListValueExpr
>                         ,comma
>                         ,parens
>                         ) where
>
> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Char (oneOf, digit, string, anyChar, char, letter)
> import Text.Parsec.String.Combinator (many1, manyTill, eof, choice, between
>                                      ,sepBy, optionMaybe)
> import Text.Parsec.String.Parsec (try,parse)
>
> import Control.Applicative (many, (<*),(<$>), (*>), (<|>),(<$),(<*>))
> import Control.Monad (void,guard)
> import qualified Text.Parsec.String.Expr as E
> import qualified Test.HUnit as H
> import FunctionsAndTypesForParsing
>
> import Debug.Trace (trace)

= Lineup

== comments

We will start supporting comments. It will support the two standard
comment syntaxes from the standard:

```sql
-- single line comment
/*
multiline
comment
*/
```

The `/* */` comments do not nest.

== literals

It will just support positive integral and string literals at this
time. Proper SQL supports more literal types including some quite
weird syntax which we will skip for now.

```sql
1
500
'string literal'
```

== identifiers

We will use simple identifiers: an identifier may start with a letter
or underscore, and contain letters, underscores and numbers. Full SQL
identifiers are more complicated to support so we will skip this for
now also.

```sql
a
something
_test_
a123
```

== 'dotted identifiers'

We will do some limited support for identifiers with two parts
separated by a dot. I don't want to get into the exact meaning or the
various names used to describe these since it is a bit confusing,
especially in SQL. We can just stick to the syntax. Both parts must
parse according to the identifier rules above.

```sql
t.a
something.something_else
```

== star

We will support the star as special expression which can be used at
the top level of select lists (and a few other places in SQL). We will
also support a 'dotted star'.

```sql
*
t.*
```

== function application

This represents any syntax which looks like the normal function
application used in languages like C. The function name must parse as
a valid identifier according to the rules above.

```sql
f()
g(1)
h(2,'something')
```

== operators

We will only support a small range of binary operators for now plus a
single prefix unary operator (`not`). We will attempt to support
correct precedence and associativity for these via the
Text.Parsec.Expr module. Here is a complete list of all the supported
operators.

```sql
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
```

== case expression

There are two standard variations of case expressions in SQL. One is
more like a switch statement in C (but is an expression, not a
statement):

```sql
case a
when 3 then 'got three'
when 5 then 'got five'
else 'neither'
end
```

The other has a boolean expression in each branch:

```sql
case
when a = 3 then 'a is three'
when b = 4 then 'b is four'
else 'neither'
end
```

The else branch is optional (if it is missing, it implicitly means
'else null').

== parentheses

It will parse and represent parentheses explicitly in the abstract
syntax, like we did with the previous expression parsers.

```sql
(1 + 2) * 3
```
= abstract syntax for value expressions

Here is a type to represent value expressions:

> data ValueExpr = StringLit String
>                | NumLit Integer
>                | Iden String
>                | DIden String String -- a.b
>                | Star
>                | DStar String -- t.*
>                | App String [ValueExpr]
>                | PrefOp String ValueExpr
>                | BinOp ValueExpr String ValueExpr
>                | Case (Maybe ValueExpr) -- test value
>                       [(ValueExpr,ValueExpr)] -- when branches
>                       (Maybe ValueExpr) -- else value
>                | Parens ValueExpr
>                  deriving (Eq,Show)

Here is the plan for tackling this:

Let's write some simple automated tests to check our progress and
check for regressions.

We can start by using the code already written to produce a partial
expression parser, then add parsing for each new constructor one at a
time.

= automated testing framework

Let's start with some examples we can turn into automated tests. Here
are the constructors above which I think we've already more or less
implemented the parsers for in previous tutorials: `NumLit`, `Iden`,
`PrefixOp`, `BinaryOp` and `Parens`.

> numLitTests :: [(String,ValueExpr)]
> numLitTests =
>     [("1", NumLit 1)
>     ,("54321", NumLit 54321)]
>
> idenTests :: [(String,ValueExpr)]
> idenTests =
>     [("test", Iden "test")
>     ,("_something3", Iden "_something3")]
>
> operatorTests :: [(String,ValueExpr)]
> operatorTests =
>    map (\o -> (o ++ " a", PrefOp o (Iden "a"))) ["not", "+", "-"]
>    ++ map (\o -> ("a " ++ o ++ " b", BinOp (Iden "a") o (Iden "b")))
>           ["=",">","<", ">=", "<=", "!=", "<>"
>           ,"and", "or", "+", "-", "*", "/", "||", "like"]
>
> parensTests :: [(String,ValueExpr)]
> parensTests = [("(1)", Parens (NumLit 1))]

> basicTests :: [(String,ValueExpr)]
> basicTests = numLitTests ++ idenTests ++ operatorTests ++ parensTests

Here is a test runner which uses HUnit:

> makeTest :: (Eq a, Show a) => Parser a -> (String,a) -> H.Test
> makeTest parser (src,expected) = H.TestLabel src $ H.TestCase $ do
>     let gote = parse (whitespace *> parser <* eof) "" src
>     case gote of
>       Left e -> H.assertFailure $ show e
>       Right got -> H.assertEqual src expected got

= the parsing we already have

== tokens

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
> symbol s = try $ lexeme $ do
>     u <- many1 (oneOf "<>=+-^%/*!|")
>     guard (s == u)
>     return s

> openParen :: Parser Char
> openParen = lexeme $ char '('

> closeParen :: Parser Char
> closeParen = lexeme $ char ')'

The whitespace parser is below, since it includes some new code to
deal with comments.

== helper functions

> keyword :: String -> Parser String
> keyword k = try $ do
>     i <- identifier
>     guard (i == k)
>     return k

TODO: find a place to discuss putting try in the keyword and symbol
parsers. I think maybe this should come in the error message tutorial?

> parens :: Parser a -> Parser a
> parens = between openParen closeParen

== terms

> num :: Parser ValueExpr
> num = NumLit <$> integer

> iden :: Parser ValueExpr
> iden = Iden <$> identifier

I'm going to parameterize the parens parser again to avoid rewriting
lots of very similar code in this tutorial.

> parensValue :: Parser ValueExpr -> Parser ValueExpr
> parensValue val = Parens <$> parens val

> term0 :: Parser ValueExpr
> term0 = iden <|> num <|> parensValue valueExpr0

== operator table and the first value expression parser

I've added all the new operators in this table.

> table :: [[E.Operator ValueExpr]]
> table = [[prefix "-", prefix "+"]
>          ,[binary "^" E.AssocLeft]
>          ,[binary "*" E.AssocLeft
>           ,binary "/" E.AssocLeft
>           ,binary "%" E.AssocLeft]
>          ,[binary "+" E.AssocLeft
>           ,binary "-" E.AssocLeft]
>          ,[binary "<=" E.AssocRight
>           ,binary ">=" E.AssocRight
>           ,binaryK "like" E.AssocNone
>           ,binary "!=" E.AssocRight
>           ,binary "<>" E.AssocRight
>           ,binary "||" E.AssocRight]
>          ,[binary "<" E.AssocNone
>           ,binary ">" E.AssocNone]
>          ,[binary "=" E.AssocRight]
>          ,[prefixK "not"]
>          ,[binaryK "and" E.AssocLeft]
>          ,[binaryK "or" E.AssocLeft]]
>   where
>     binary name assoc =
>         E.Infix (mkBinOp name <$ symbol name) assoc
>     mkBinOp nm a b = BinOp a nm b
>     prefix name = E.Prefix (PrefOp name <$ symbol name)
>     binaryK name assoc =
>         E.Infix (mkBinOp name <$ keyword name) assoc
>     prefixK name = E.Prefix (PrefOp name <$ keyword name)

> valueExpr0 :: Parser ValueExpr
> valueExpr0 = E.buildExpressionParser table term0

Now we can run the tests:

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest valueExpr0) basicTests
Cases: 23  Tried: 23  Errors: 0  Failures: 0
Counts {cases = 23, tried = 23, errors = 0, failures = 0}
```

= new parsers

Let's start extending things one constructor at a time, but first we
will do comments.

== whitespace and comments

Here is our old whitespace parser:

> whitespace0 :: Parser ()
> whitespace0 = void $ many $ oneOf " \n\t"

We are going to change this to do comments as well.

TODO: build these two parsers up in stages, show why each bit is there

> lineComment :: Parser ()
> lineComment = void (try (string "--") *>
>                     manyTill anyChar (void (char '\n') <|> eof))

> blockComment :: Parser ()
> blockComment = void (-- no nesting of block comments in SQL
>                      try (string "/*")
>                      -- TODO: why is try used here
>                      *> manyTill anyChar (try $ string "*/"))

todo: discuss how to compose to create the whitespace parser
todo: create the whitespace parser without many1 and return () and
show the problem.

Here is the final parser for whitespace:

> whitespace :: Parser ()
> whitespace =
>     choice [simpleWhitespace *> whitespace
>            ,lineComment *> whitespace
>            ,blockComment *> whitespace
>            ,return ()]
>   where
>     lineComment = try (string "--")
>                   *> manyTill anyChar (void (char '\n') <|> eof)
>     blockComment = try (string "/*")
>                    *> manyTill anyChar (try $ string "*/")
>     simpleWhitespace = void $ many1 (oneOf " \t\n")

== string literal

Our string literal is any characters except single quote enclosed in
single quotes. We aren't going to support other string literal
syntaxes or escaping single quotes within a string right now.

Here are some examples:

> stringLiteralTests :: [(String,ValueExpr)]
> stringLiteralTests =
>     [("''", StringLit "")
>     ,("'test'", StringLit "test")]

We need a new token parser for string literals:

> stringToken :: Parser String
> stringToken = lexeme (char '\'' *> manyTill anyChar (char '\''))

TODO: more explanation

And the string literal expression parser:

> stringLit :: Parser ValueExpr
> stringLit = StringLit <$> stringToken

Here is the new value expression parser:

> term1 :: Parser ValueExpr
> term1 = iden <|> num <|> parensValue valueExpr1 <|> stringLit

> valueExpr1 :: Parser ValueExpr
> valueExpr1 = E.buildExpressionParser table term1

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest valueExpr1) (basicTests ++ stringLiteralTests)
Cases: 25  Tried: 25  Errors: 0  Failures: 0
Counts {cases = 25, tried = 25, errors = 0, failures = 0}
```

So far, so good.

== dotted identifier

> dIdenTests :: [(String,ValueExpr)]
> dIdenTests =
>     [("t.a", DIden "t" "a")]

Here is a new token parser to use:

> dot :: Parser Char
> dot = lexeme $ char '.'

> dIden :: Parser ValueExpr
> dIden = DIden <$> identifier <*> (dot *> identifier)

> term2 :: Parser ValueExpr
> term2 = iden <|> num <|> parensValue valueExpr2
>         <|> stringLit <|> dIden

> valueExpr2 :: Parser ValueExpr
> valueExpr2 = E.buildExpressionParser table term2

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest valueExpr2) (basicTests ++ stringLiteralTests ++ dIdenTests)
 ### Failure in: 25:t.a
(line 1, column 2):
unexpected '.'
expecting digit, letter, "_", "--", "/*", operator or end of input
Cases: 26  Tried: 26  Errors: 0  Failures: 1
Counts {cases = 26, tried = 26, errors = 0, failures = 1}
```

Do you know why this happened? Can you think of a way to solve this?

Here is the usual blunt hammer technique: reorder the choices to put
the longest choice first and use `try` when there is a common prefix.

> term3 :: Parser ValueExpr
> term3 = try dIden <|> iden <|> num <|> parensValue valueExpr3
>         <|> stringLit

> valueExpr3 :: Parser ValueExpr
> valueExpr3 = E.buildExpressionParser table term3

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest valueExpr3) (basicTests ++ stringLiteralTests ++ dIdenTests)
Cases: 26  Tried: 26  Errors: 0  Failures: 0
Counts {cases = 26, tried = 26, errors = 0, failures = 0}
```
== star

star - no surprises

> starTests :: [(String,ValueExpr)]
> starTests = [("*", Star)]

> star :: Parser ValueExpr
> star = Star <$ symbol "*"

> term4 :: Parser ValueExpr
> term4 = try dIden <|> iden <|> num <|> parensValue valueExpr4
>         <|> stringLit <|> star

> valueExpr4 :: Parser ValueExpr
> valueExpr4 = E.buildExpressionParser table term4

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest valueExpr4) (basicTests ++ stringLiteralTests ++ dIdenTests ++ starTests)
Cases: 27  Tried: 27  Errors: 0  Failures: 0
Counts {cases = 27, tried = 27, errors = 0, failures = 0}
```

== dotted star

Here is dotted star.

> dStarTests :: [(String,ValueExpr)]
> dStarTests = [("t.*", DStar "t")]

> dstar :: Parser ValueExpr
> dstar = DStar <$> (identifier <* dot <* symbol "*")

We'll have the same issue we've seen before, so let's go straight to
the solution.

> term5 :: Parser ValueExpr
> term5 = try dstar <|> try dIden <|> iden <|> num
>         <|> parensValue valueExpr5 <|> stringLit <|> star

> valueExpr5 :: Parser ValueExpr
> valueExpr5 = E.buildExpressionParser table term5

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest valueExpr5) (basicTests ++ stringLiteralTests ++ dIdenTests ++ starTests ++ dStarTests)
Cases: 28  Tried: 28  Errors: 0  Failures: 0
Counts {cases = 28, tried = 28, errors = 0, failures = 0}
```

== app

The App constructor is used for syntax which looks like regular
function application: f(), f(a), f(a,b), etc.

> appTests :: [(String,ValueExpr)]
> appTests = [("f()", App "f" [])
>            ,("f(1)", App "f" [NumLit 1])
>            ,("f(1,a)", App "f" [NumLit 1, Iden "a"])]

Here is the parser, parameterized like the `parensValue` parser for
the same reason:

> app :: Parser ValueExpr -> Parser ValueExpr
> app val = App <$> identifier <*> parens (commaSep val)

And here is the `commaSep` helper:

> commaSep :: Parser a -> Parser [a]
> commaSep = (`sepBy` comma)

> comma :: Parser Char
> comma = lexeme $ char ','

It is another parser with the `identifier` prefix, so another `try`.

> term6 :: Parser ValueExpr
> term6 = try (app valueExpr6) <|> try dstar <|> try dIden <|> iden
>         <|> num <|> parensValue valueExpr6 <|> stringLit <|> star

> valueExpr6 :: Parser ValueExpr
> valueExpr6 = E.buildExpressionParser table term6

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest valueExpr6) (basicTests ++ stringLiteralTests ++ dIdenTests ++ starTests ++ dStarTests ++ appTests)
Cases: 31  Tried: 31  Errors: 0  Failures: 0
Counts {cases = 31, tried = 31, errors = 0, failures = 0}
```

Everything looks good.

== case

Here is something a little more interesting.

> caseTests :: [(String,ValueExpr)]
> caseTests =
>     [("case a when 1 then 2 end"
>      ,Case (Just $ Iden "a") [(NumLit 1,NumLit 2)] Nothing)
>
>     ,("case a when 1 then 2 when 3 then 4 end"
>      ,Case (Just $ Iden "a")
>            [(NumLit 1, NumLit 2)
>            ,(NumLit 3, NumLit 4)]
>            Nothing)
>
>     ,("case a when 1 then 2 when 3 then 4 else 5 end"
>      ,Case (Just $ Iden "a")
>            [(NumLit 1, NumLit 2)
>            ,(NumLit 3, NumLit 4)]
>            (Just $ NumLit 5))
>
>     ,("case when a=1 then 2 when a=3 then 4 else 5 end"
>      ,Case Nothing
>            [(BinOp (Iden "a") "=" (NumLit 1), NumLit 2)
>            ,(BinOp (Iden "a") "=" (NumLit 3), NumLit 4)]
>            (Just $ NumLit 5))
>     ]

How can we approach this? We know that there will always be a case
keyword at the start, and an end keyword at the end. Each when branch
seems to be self contained.

Here is a rough pseudo-code sketch:

```
keyword "case"
optional test expression
many1 when clause
optional else clause
keyword "end
```

Here is the case parser based simply on this pseudo-code.

> caseValue0 :: Parser ValueExpr -> Parser ValueExpr
> caseValue0 val = do
>     void $ keyword "case"
>     testExp <- optionMaybe val
>     whens <- many1 whenClause
>     els <- optionMaybe elseClause
>     void $ keyword "end"
>     return $ Case testExp whens els
>  where
>    whenClause = (,) <$> (keyword "when" *> val)
>                     <*> (keyword "then" *> val)
>    elseClause = keyword "else" *> val

Let's try it on its own first and see if we have any problems:

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest (caseValue0 valueExpr6)) caseTests in: 3:case when a=1 then 2 when a=3 then 4 else 5 end
(line 1, column 12):
unexpected "="
expecting operator, digit, letter, "_", "--" or "/*"
Cases: 4  Tried: 4  Errors: 0  Failures: 1
Counts {cases = 4, tried = 4, errors = 0, failures = 1}
```

Three work, and one fails. What happened here? Try to run the parsing
code in your head to see if you can work out what the problem is.

Here is a method we can use when you can't manage to locate a problem
in this way.

Let's single out the failure first:

```
*ValueExpressions> parseWithEof (caseValue0 valueExpr6) "case when a=1 then 2 when a=3 then 4 else 5 end"
Left (line 1, column 12):
unexpected "="
expecting operator, digit, letter, "_", "--" or "/*"
```

Here is the `caseValue` parser rewritten with `trace` interleaved:

> caseValue1 :: Parser ValueExpr -> Parser ValueExpr
> caseValue1 val = do
>     trace "start case" $ return ()
>     void $ keyword "case"
>     trace "read case keyword" $ return ()
>     testExp <- optionMaybe val
>     trace ("read testExpr: " ++ show testExp) $ return ()
>     whens <- many1 whenClause
>     trace ("read whens: " ++ show (length whens)) $ return ()
>     els <- optionMaybe elseClause
>     trace ("read else: " ++ show els) $ return ()
>     void $ keyword "end"
>     trace "read end keyword" $ return ()
>     return $ Case testExp whens els
>  where
>    whenClause = do
>        trace "start when clause" $ return ()
>        void $ keyword "when"
>        trace "read when keyword" $ return ()
>        w <- val
>        trace ("read when exp: " ++ show w) $ return ()
>        void $ keyword "then"
>        trace "read then keyword" $ return ()
>        t <- val
>        trace ("read then exp: " ++ show t) $ return ()
>        return (w,t)
>    elseClause = do
>        trace "start else clause" $ return ()
>        void $ keyword "else"
>        trace "read else keyword" $ return ()
>        v <- val
>        trace ("read else exp: " ++ show v) $ return ()
>        return v

Now when we run the test, we can see a trace of what happens:

```
*ValueExpressions> parseWithEof (caseValue1 valueExpr6) "case when a=1 then 2 when a=3 then 4 else 5 end"
start case
read case keyword
read testExpr: Just (Iden "when")
start when clause
Left (line 1, column 12):
unexpected "="
expecting operator, digit, letter, "_", "--" or "/*"
```

Look at the trace and see if you spot the problem.

We've parsed the `when` keyword as an identifier for the optional
first expression. The way we deal with this is to use a new identifier
parser which has a blacklist of keywords which can't be identifiers.

> caseValue2 :: Parser ValueExpr -> Parser ValueExpr
> caseValue2 val = do
>     trace "start case" $ return ()
>     void $ keyword "case"
>     trace "read case keyword" $ return ()
>     testExp <- optionMaybe caseVal
>     trace ("read testExpr: " ++ show testExp) $ return ()
>     whens <- many1 whenClause
>     trace ("read whens: " ++ show (length whens)) $ return ()
>     els <- optionMaybe elseClause
>     trace ("read else: " ++ show els) $ return ()
>     void $ keyword "end"
>     trace "read end keyword" $ return ()
>     return $ Case testExp whens els
>  where
>    whenClause = do
>        trace "start when clause" $ return ()
>        void $ keyword "when"
>        trace "read when keyword" $ return ()
>        w <- caseVal
>        trace ("read when exp: " ++ show w) $ return ()
>        void $ keyword "then"
>        trace "read then keyword" $ return ()
>        t <- caseVal
>        trace ("read then exp: " ++ show t) $ return ()
>        return (w,t)
>    elseClause = do
>        trace "start else clause" $ return ()
>        void $ keyword "else"
>        trace "read else keyword" $ return ()
>        v <- caseVal
>        trace ("read else exp: " ++ show v) $ return ()
>        return v
>    -- here is the fix, we replace calls to `val` with calls to
>    -- this function which prevents any of the case keywords from
>    -- being parsed as identifiers
>    caseVal = try $ do
>        v <- val
>        guard $ case v of
>            Iden i | i `elem` ["case", "when", "then", "else", "end"] -> False
>            _ -> True
>        return v

Maybe this blacklist used in this way isn't permissive enough, or is
too permissive, but let's not get lost in these details right now, and
come back to it later.

```
*ValueExpressions> parseWithEof (caseValue2 valueExpr6) "case when a=1 then 2 when a=3 then 4 else 5 end"
start case
read case keyword
read testExpr: Nothing
start when clause
read when keyword
read when exp: BinOp (Iden "a") "=" (NumLit 1)
read then keyword
read then exp: NumLit 2
read when exp: BinOp (Iden "a") "=" (NumLit 3)
read then keyword
read then exp: NumLit 4
read whens: 2
start else clause
read else keyword
read else exp: NumLit 5
read else: Just (NumLit 5)
read end keyword
Right (Case Nothing [(BinOp (Iden "a") "=" (NumLit 1),NumLit 2),(BinOp (Iden "a") "=" (NumLit 3),NumLit 4)] (Just (NumLit 5)))
```

Looks good. Have a close look through the trace to see if you can
follow it all and it makes sense. TODO: There appear to be some
messages missing - I think it is some sort of memoization effect.

Let's run all the tests with the tracing still in so if there are any
failures we can zero in on them.

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest (caseValue2 valueExpr6)) caseTests
Cases: 4  Tried: 0  Errors: 0  Failures: 0start case
read case keyword
read testExpr: Just (Iden "a")
start when clause
read when keyword
read when exp: NumLit 1
read then keyword
read then exp: NumLit 2
read whens: 1
start else clause
read else: Nothing
read end keyword
Cases: 4  Tried: 1  Errors: 0  Failures: 0read testExpr: Just (Iden "a")
read when exp: NumLit 1
read then keyword
read then exp: NumLit 2
read when exp: NumLit 3
read then keyword
read then exp: NumLit 4
read whens: 2
read else: Nothing
read end keyword
Cases: 4  Tried: 2  Errors: 0  Failures: 0read testExpr: Just (Iden "a")
read when exp: NumLit 1
read then keyword
read then exp: NumLit 2
read when exp: NumLit 3
read then keyword
read then exp: NumLit 4
read whens: 2
read else keyword
read else exp: NumLit 5
read else: Just (NumLit 5)
read end keyword
Cases: 4  Tried: 3  Errors: 0  Failures: 0read testExpr: Nothing
read when exp: BinOp (Iden "a") "=" (NumLit 1)
read then keyword
read then exp: NumLit 2
read when exp: BinOp (Iden "a") "=" (NumLit 3)
read then keyword
read then exp: NumLit 4
read whens: 2
read else exp: NumLit 5
read else: Just (NumLit 5)
read end keyword
Cases: 4  Tried: 4  Errors: 0  Failures: 0
Counts {cases = 4, tried = 4, errors = 0, failures = 0}
```

Now the full expression parser:

> term7 :: Parser ValueExpr
> term7 = caseValue2 valueExpr7
>         <|> try (app valueExpr7) <|> try dstar <|> try dIden <|> iden
>         <|> num <|> parensValue valueExpr7 <|> stringLit <|> star

> valueExpr7 :: Parser ValueExpr
> valueExpr7 = E.buildExpressionParser table term7

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest valueExpr7) (basicTests ++ stringLiteralTests ++ dIdenTests ++ starTests ++ dStarTests ++ appTests ++ caseTests)
Cases: 35  Tried: 0  Errors: 0  Failures: 0start case
Cases: 35  Tried: 31  Errors: 0  Failures: 0read case keyword
read testExpr: Just (Iden "a")
start when clause
read when keyword
read when exp: NumLit 1
read then keyword
read then exp: NumLit 2
read whens: 1
start else clause
read else: Nothing
read end keyword
Cases: 35  Tried: 32  Errors: 0  Failures: 0read testExpr: Just (Iden "a")
read when exp: NumLit 1
read then keyword
read then exp: NumLit 2
read when exp: NumLit 3
read then keyword
read then exp: NumLit 4
read whens: 2
read else: Nothing
read end keyword
Cases: 35  Tried: 33  Errors: 0  Failures: 0read testExpr: Just (Iden "a")
read when exp: NumLit 1
read then keyword
read then exp: NumLit 2
read when exp: NumLit 3
read then keyword
read then exp: NumLit 4
read whens: 2
read else keyword
read else exp: NumLit 5
read else: Just (NumLit 5)
read end keyword
Cases: 35  Tried: 34  Errors: 0  Failures: 0read testExpr: Nothing
read when exp: BinOp (Iden "a") "=" (NumLit 1)
read then keyword
read then exp: NumLit 2
read when exp: BinOp (Iden "a") "=" (NumLit 3)
read then keyword
read then exp: NumLit 4
read whens: 2
read else exp: NumLit 5
read else: Just (NumLit 5)
read end keyword
Cases: 35  Tried: 35  Errors: 0  Failures: 0
Counts {cases = 35, tried = 35, errors = 0, failures = 0}
```

Everything looks good. Let's refactor the case parser and tidy
everything up.

> blackListValueExpr :: [String] -> Parser ValueExpr -> Parser ValueExpr
> blackListValueExpr blackList val = try $ do
>     v <- val
>     guard $ case v of
>         Iden i | i `elem` blackList -> False
>         _ -> True
>     return v

Here is the parser with the `trace`s removed, and using the new
`blackListValueExpr` parser.

> caseValue3 :: Parser ValueExpr -> Parser ValueExpr
> caseValue3 val = do
>     void $ keyword "case"
>     testExp <- optionMaybe caseVal
>     whens <- many1 whenClause
>     els <- optionMaybe elseClause
>     void $ keyword "end"
>     return $ Case testExp whens els
>  where
>    whenClause = do
>        void $ keyword "when"
>        w <- caseVal
>        void $ keyword "then"
>        t <- caseVal
>        return (w,t)
>    elseClause = do
>        void $ keyword "else"
>        v <- caseVal
>        return v
>    caseVal = blackListValueExpr blackList val
>    blackList = ["case", "when", "then", "else", "end"]

And here it is after some shortening:

> caseValue4 :: Parser ValueExpr -> Parser ValueExpr
> caseValue4 val =
>     Case
>     <$> (keyword "case" *> optionMaybe caseVal)
>     <*> many1 whenClause
>     <*> optionMaybe elseClause
>     <* keyword "end"
>  where
>    whenClause = (,) <$> (keyword "when" *> caseVal)
>                     <*> (keyword "then" *> caseVal)
>    elseClause = keyword "else" *> caseVal
>    caseVal = blackListValueExpr blackList val
>    blackList = ["case", "when", "then", "else", "end"]

I think we passed the point where `(<|>)` is more readable than
`choice`:

> term8 :: Parser ValueExpr
> term8 = choice [caseValue4 valueExpr8
>                ,try $ app valueExpr8
>                ,try dstar
>                ,try dIden
>                ,iden
>                ,num
>                ,parensValue valueExpr8
>                ,stringLit
>                ,star]

> valueExpr8 :: Parser ValueExpr
> valueExpr8 = E.buildExpressionParser table term8

> allExpressionTests :: [(String,ValueExpr)]
> allExpressionTests = concat [basicTests
>                             ,stringLiteralTests
>                             ,dIdenTests
>                             ,starTests
>                             ,dStarTests
>                             ,appTests
>                             ,caseTests]

Let's double check:

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest valueExpr8) allExpressionTests
Cases: 35  Tried: 35  Errors: 0  Failures: 0
Counts {cases = 35, tried = 35, errors = 0, failures = 0}
```

TODO: review code above for new syntax patterns to talk about. Or
maybe don't talk about them?

Let's predict that we will need more blacklisting when we work on the
query expression parsing, and create a parser which can be used in
this way:

> term :: [String] -> Parser ValueExpr
> term blackList = choice [caseValue4 (valueExpr blackList)
>                         ,try (app (valueExpr blackList))
>                         ,try dstar
>                         ,try dIden
>                         ,blackListValueExpr blackList iden
>                         ,num
>                         ,parensValue (valueExpr blackList)
>                         ,stringLit
>                         ,star]

TODO: need to change app, dstar and diden for blacklisting?

> valueExpr :: [String] -> Parser ValueExpr
> valueExpr blackList = E.buildExpressionParser table (term blackList)

Final sanity check:

```
*ValueExpressions> H.runTestTT $ H.TestList $ map (makeTest (valueExpr [])) allExpressionTests
Cases: 35  Tried: 35  Errors: 0  Failures: 0
Counts {cases = 35, tried = 35, errors = 0, failures = 0}
```

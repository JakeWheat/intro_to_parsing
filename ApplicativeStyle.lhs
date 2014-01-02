
= tidying things up

Now we can go back over the code already written, and make it much
more concise, whilst also trying to make it more readable. We are
going to use the typeclass Applicative and some functions from this
typeclass.

> module ApplicativeStyle where
>
> import Text.Parsec (ParseError)
> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Parsec (try)
> import Text.Parsec.String.Char (oneOf, char, digit
>                                ,string, letter, satisfy)
> import Text.Parsec.String.Combinator (many1, choice, chainl1, between)
> import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), (<|>), many)
> import Control.Monad (void, ap)
> import Data.Char (isLetter, isDigit)
> import Boilerplate
> import FirstRealParsing


Here is the basic pattern behind a lot of this. Here is a function
which takes a constructor and two parsers for the two arguments for
the constructor. It parses the two arguments, then applies the
constructor to the results:

> myParser1 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
> myParser1 ctor pa pb = do
>     a <- pa
>     b <- pb
>     return $ ctor a b

TODO: concrete example

This can be rewritten without the do syntactic sugar like this:

> myParser2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
> myParser2 ctor pa pb =
>     pa >>= \a -> pb >>= \b -> return $ ctor a b

And this can be rewritten like this:

> myParser3 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
> myParser3 ctor pa pb = ctor `fmap` pa `ap` pb

(This uses functions from Applicative instead of Monad). We replace
the use of `>>=` with `fmap` and `ap`. This isn't always possible, but
it often is.

Here is the version using the operator versions (`fmap` changed to `<$>`,
and `ap` changed to `<*>`). These two operators are just alternative
spellings of fmap and ap.

> myParser4 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
> myParser4 ctor pa pb = ctor <$> pa <*> pb

This style takes less typing, and is often much simpler to write and
read.

This pattern 'scales', you can use:

```
Ctor <$> pa
```

for a single argument constructor. This might also be familiar to you
as

```
fmap Ctor pa
```

or

```
Ctor `fmap` pa
```

All of which mean the same thing, just slightly different spellings.

This can also be written using Monad operators:

```
pa >>= liftM ctor
```

or

```
liftM ctor =<< pa
```

(`liftM` is in Control.Monad)

These `liftM` versions effectively mean the same thing as the previous
versions with `fmap` and `<$>`.

You can use

```
Ctor <$> pa <*> pb <*> pc
```

for three args, and so on. So you use `<$>` between the pure
constructor and the first arg, then `<*>` between each subsequent arg.

Let's go over the simple expression parsers and try to rewrite them
using this style. We will see a few other new functions. I will break
things down into a lot of steps.

== num

Here was the old parser.

> numA0 :: Parser SimpleExpr
> numA0 = lexeme $ do
>    n <- many1 digit
>    return $ Num $ read n

Let's rewrite it in steps. The first step is to move the 'read' to the
first line using `<$>`. Maybe it is too obvious to state explicitly,
but I will do it anyway: you can use any pure function with `<$>` and
not just constructors.

> numA1 :: Parser SimpleExpr
> numA1 = lexeme $ do
>     n <- read <$> many1 digit
>     return $ Num n

Now we can move the Num constructor:

> numA2 :: Parser SimpleExpr
> numA2 = lexeme $ do
>     n <- Num . read <$> many1 digit
>     return n

How can we get rid of the whitespace. Here is an additional operator
`(<*)`, which can be used:

```
a <* b
```

is equivalent to

```
do
    x <- a
    void b
    return x
```

TODO: added the lexeme function, so lost this code. Redo it here in a
separate set of step.

Here it is in use in the function.

> numA3 :: Parser SimpleExpr
> numA3 = lexeme $ do
>     n <- Num . read <$> many1 digit
>     return n

Now we can apply the usual monad rewrite laws:

> numA4 :: Parser SimpleExpr
> numA4 = lexeme (Num . read <$> many1 digit)

In more industrial parser code, I would usually write some
'tokenization' parsers separately like this:

> integerA5 :: Parser Integer
> integerA5 = lexeme (read <$> many1 digit)

TODO: should this read 'read <$> lexeme (many1 digit)\'? Can this
option possibly matter?

Then the num expression parser looks like this:

> numA5 :: Parser SimpleExpr
> numA5 = Num <$> integerA5

and we get a integer parser which we can reuse if we need to parse an
integer in another context.

== var

here is the old var parser:

```
varD :: Parser SimpleExpr
varD = lexeme $ do
    fl <- firstChar
    rest <- many nonFirstChar
    return $ Var (fl:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')
```

The first thing we can do is to make the firstChar and nonFirstChar a
little easier to read:

> varA0 :: Parser SimpleExpr
> varA0 = lexeme $ do
>     fl <- firstChar
>     rest <- many nonFirstChar
>     return $ Var (fl:rest)
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

Now we can lift the (:) using the Applicative operators.

> varA1 :: Parser SimpleExpr
> varA1 = do
>     i <- iden
>     return $ Var i
>   where
>     iden = lexeme ((:) <$> firstChar <*> many nonFirstChar)
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

And tidy it up using <$> with the Var constructor:

> varA2 :: Parser SimpleExpr
> varA2 = Var <$> iden
>   where
>     iden = lexeme ((:) <$> firstChar <*> many nonFirstChar)
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

It looks almost like a grammar description now.

== parens

Here is the starting point:

```
parensD :: Parser SimpleExpr
parensD = do
    void $ lexeme $ char '('
    e <- simpleExprD
    void $ lexeme $ char ')'
    return $ Parens e
```

Here you can see that there is a `(*>)` which works in the opposite
direction to `(<*)`. The precendence of these operators means that we
have to use some extra parentheses (!) here:

> parensA0 :: Parser SimpleExpr
> parensA0 =
>     Parens <$> (lexeme (char '(') *> simpleExprD <* lexeme (char ')'))

TODO: lost the chained <*. Put something below about this so there is
a concrete example

== simple expr

Here is the old version

```
simpleExprD :: Parser SimpleExpr
simpleExprD = do
    e <- term
    maybeAddSuffix e
  where
    maybeAddSuffix e = addSuffix e <|> return e
    addSuffix e0 = do
        void $ lexeme $ char '+'
        e1 <- term
        maybeAddSuffix (Add e0 e1)
    term = numD <|> varD <|> parensD
```

Start with the function body, convert to point free style.

> simpleExprA0 :: Parser SimpleExpr
> simpleExprA0 = term >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 = do
>         void $ lexeme $ char '+'
>         e1 <- term
>         maybeAddSuffix (Add e0 e1)
>     term = numD <|> varD <|> parensD

Now rewrite the main part of the addSuffix function:

> simpleExprA1 :: Parser SimpleExpr
> simpleExprA1 = term >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 = do
>         e1 <- lexeme (char '+') *> term
>         maybeAddSuffix (Add e0 e1)
>     term = numD <|> varD <|> parensD

now combine the Add ctor call into one line

> simpleExprA2 :: Parser SimpleExpr
> simpleExprA2 = term >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 = do
>         ae <- Add e0 <$> (lexeme (char '+') *> term)
>         maybeAddSuffix ae
>     term = numD <|> varD <|> parensD

now simplify the addSuffix function to make it point free.

> simpleExprA3 :: Parser SimpleExpr
> simpleExprA3 = term >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 =
>         Add e0 <$> (lexeme (char '+') *> term)
>         >>= maybeAddSuffix
>     term = numD <|> varD <|> parensD

TODO: use chainl here and an additional step. Alter the code lower
down to match this change.

== summary

Here is the finished job for all the simple expression code.

> numA' :: Parser SimpleExpr
> numA' = lexeme (Num . read <$> many1 digit)

> varA' :: Parser SimpleExpr
> varA' = Var <$> lexeme ((:) <$> firstChar <*> many nonFirstChar)
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

> parensA' :: Parser SimpleExpr
> parensA' =
>     Parens <$> (lexeme (char '(') *> simpleExprA' <* lexeme (char ')'))

> simpleExprA' :: Parser SimpleExpr
> simpleExprA' = term >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 =
>         (Add e0 <$> (lexeme (char '+') *> term)) >>= maybeAddSuffix
>     term = numA' <|> varA' <|> parensA'

Here a version with the separate token parsers.

The token parsers:

> lexemeA :: Parser a -> Parser a
> lexemeA p = p <* whiteSpace

> identifier :: Parser String
> identifier = lexemeA ((:) <$> firstChar <*> many nonFirstChar)
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

> integer :: Parser Integer
> integer = read <$> lexemeA (many1 digit)

> symbol :: Char -> Parser ()
> symbol c = void $ lexemeA $ char c

Here is another little helper function. It barely pays its way in this
short example, but even though it is only used once, I think it is
worth it to make the code clearer.

> betweenParens :: Parser a -> Parser a
> betweenParens p = symbol '(' *> p <* symbol ')'

The expression parsers:

> numA :: Parser SimpleExpr
> numA = Num <$> integer

> varA :: Parser SimpleExpr
> varA = Var <$> identifier

> parensA :: Parser SimpleExpr
> parensA = Parens <$> betweenParens simpleExprA

> simpleExprA :: Parser SimpleExpr
> simpleExprA = term >>= maybeAddSuffix
>   where
>     maybeAddSuffix e = addSuffix e <|> return e
>     addSuffix e0 = (Add e0 <$> (symbol '+' *> term)) >>= maybeAddSuffix
>     term = numA <|> varA <|> parensA

Splitting the lexer parser layer out means that we have one place
where we have to remember to add the ignore whitespace suffixes, and I
think makes the code a bit simpler to follow.

TODO: this chain stuff should be above in the simpleexpr rewrite steps

Here is a version of simpleExprA using the chainl1 function from
Text.Parsec.Combinator:

> simpleExprAC :: Parser SimpleExpr
> simpleExprAC = chainl1 term op
>   where
>     op = symbol '+' *> return Add
>     term = numA <|> varA <|> parensA

The chainl1 simplified type is like this:

```
Parser a -> Parser (a -> a -> a) -> Parser a
```

You pass it the function to parse a single element, then a parser
which parses the operator concrete syntax, and returns a function
which combines two elements using the operator abstract syntax, and it
does all the work.

To break it down: chainl1 will use the term parser to parse a
'term'. Then it will use the op parser. This parser must parse the
operator syntax only, and if it parses successfully, return a function
which can combine two expressions into the operator application
expression (a -> a -> a). chainl1 will then parse another term, and
then apply this function to combine the original term and the new
term.

TODO: maybe write a diagram sequence to illustrate instead.

The 'op' function here parses a plus symbol, then returns the Add
ctor, whose type signature is what we want:

```
Add :: SimpleExpr -> SimpleExpr -> SimpleExpr
```

If you look at the source of chainl1, it is more or less the code
above in simpleExprA factored out as a separate function, but written
in a different style.

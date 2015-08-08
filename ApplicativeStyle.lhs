
= Overview

Now we can go back over the expression parsing code written in the
last tutorial, and make it much more concise, and also make it more
readable. We are going to do this mainly by using functions from the
typeclass Applicative.

Remember you can (and should) use the functions `regularParse` and its
variations (TODO list them here) to try out the all these parsers in
ghci, and you can write your own variations to experiment with if you
are unsure about anything.

> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Char (oneOf, char, digit, letter, satisfy)
> import Text.Parsec.String.Combinator (many1, chainl1)
> import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
> import Control.Monad (void, ap)
> import Data.Char (isLetter, isDigit)
> import FunctionsAndTypesForParsing

Here is the SimpleExpr type again:

> data SimpleExpr = Num Integer
>                 | Var String
>                 | Add SimpleExpr SimpleExpr
>                 | Parens SimpleExpr
>                   deriving (Eq,Show)

Here is the basic pattern behind most of the rewrites we are going to
cover. Here is a function which takes a constructor and two parsers
for the two arguments for the constructor. It parses the two
arguments, then applies the constructor to the results:

> myParser1 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
> myParser1 ctor pa pb = do
>     a <- pa
>     b <- pb
>     return $ ctor a b

TODO: concrete example, plus examples at the bottom of this section
(for ctor <$> a, ctor <$> a <*> b, ctor <$> a <*> b <*> c).

This can be rewritten without the do syntactic sugar like this:

> myParser2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
> myParser2 ctor pa pb =
>     pa >>= \a -> pb >>= \b -> return $ ctor a b

And can also be rewritten like this:

> myParser3 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
> myParser3 ctor pa pb = ctor `fmap` pa `ap` pb

(This uses functions from Applicative instead of Monad.) We replace
the use of `>>=` with `fmap` and `ap`. This isn't always possible, but
it often is.

Here is the version using the operators for `fmap` and `ap` (`fmap`
changed to `<$>`, and `ap` changed to `<*>`).

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

(`liftM` is in the module `Control.Monad`)

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

= lexeme

Here is the old lexeme parser, 'D' suffix for 'do notation'.

> lexemeD :: Parser a -> Parser a
> lexemeD p = do
>            x <- p
>            whitespace
>            return x

> whitespace :: Parser ()
> whitespace = void $ many $ oneOf " \n\t"

First, we can move the `whitespace` from its own separate line.

> lexemeA0 :: Parser a -> Parser a
> lexemeA0 p = do
>            x <- p <* whitespace
>            return x

The expression `pa <* pb` means run `pa`, then run `pb`, and return
the result of `pa`. It is sort of equivalent to this code:

```haskell
(<*) :: Parser a -> Parser b -> Parser a
(<*) pa pb = do
    a <- pa
    void pb
    return a
```

(It isn't implemented this way, since `(<*)` only needs Applicative
and not Monad.)

Now we can use the usual monad syntax rewrites, first eliminate the
name `x`.

> lexemeA1 :: Parser a -> Parser a
> lexemeA1 p = do
>              p <* whitespace

Now remove the redundant do:

> lexemeA :: Parser a -> Parser a
> lexemeA p = p <* whitespace

= num

Now let's tackle the `num` parser.

> numD :: Parser SimpleExpr
> numD = do
>     n <- lexemeD $ many1 digit
>     return $ Num $ read n

Let's move 'read' to the first line.

> numA0 :: Parser SimpleExpr
> numA0 = do
>     n <- read <$> lexemeA (many1 digit)
>     return $ Num n

This uses `(<$>)` which we saw above. You may have done code rewrites
like this using `fmap` with IO in other Haskell code.

Now let's move the `Num` ctor as well:

> numA1 :: Parser SimpleExpr
> numA1 = do
>     n <- (Num . read) <$> lexemeA (many1 digit)
>     return n

You can also write it in this way:

> numA2 :: Parser SimpleExpr
> numA2 = do
>     n <- Num <$> read <$> lexemeA (many1 digit)
>     return n

Why does this work? It parses like this:

> numA2' :: Parser SimpleExpr
> numA2' = do
>     n <- Num <$> (read <$> lexemeA (many1 digit))
>     return n

Let's break it down:

> numA2'' :: Parser SimpleExpr
> numA2'' = do
>     n <- numb
>     return n
>   where
>     numb :: Parser SimpleExpr
>     numb = Num <$> int
>     int :: Parser Integer
>     int = read <$> lexemeA (many1 digit)

In terms of style, which do you think looks better:
`(a . b) <$> p` or `a <$> b <$> p`.

The next step for num, we can eliminate the temporary name `n` and the
`do`:

> numA3 :: Parser SimpleExpr
> numA3 = (Num . read) <$> lexemeA (many1 digit)

In more 'industrial' parser code, I would usually write some
tokenization parsers separately like this:

> integerA4 :: Parser Integer
> integerA4 = read <$> lexemeA (many1 digit)

Then the num expression parser looks like this:

> numA4 :: Parser SimpleExpr
> numA4 = Num <$> integerA4

and we also get a integer parser which we can reuse if we need to
parse an integer in another context.

= var

Here is the previous var parser:

> varD :: Parser SimpleExpr
> varD = lexemeA $ do
>     fc <- firstChar
>     rest <- many nonFirstChar
>     return $ Var (fc:rest)
>   where
>     firstChar = satisfy (\a -> isLetter a || a == '_')
>     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

The first thing we can do is to make the `firstChar` and
`nonFirstChar` a little easier to read, using `(<|>)`, `char`,
`letter` and `digit`:

> varA0 :: Parser SimpleExpr
> varA0 = lexemeA $ do
>     fl <- firstChar
>     rest <- many nonFirstChar
>     return $ Var (fl:rest)
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

Here is another way of making the function a little better:

> varA0' :: Parser SimpleExpr
> varA0' = lexemeA $ do
>     fl <- satisfy validFirstChar
>     rest <- many (satisfy validNonFirstChar)
>     return $ Var (fl:rest)
>   where
>     validFirstChar a = isLetter a || a == '_'
>     validNonFirstChar a = validFirstChar a || isDigit a

We can lift the `(:)` using the Applicative operators.

> varA1 :: Parser SimpleExpr
> varA1 = do
>     i <- iden
>     return $ Var i
>   where
>     iden = lexemeA ((:) <$> firstChar <*> many nonFirstChar)
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

We used the prefix version of `(:)` to use it with `(<$>)` and
`(<*>)`. The lexemeA call was moved to the `iden` helper function.

Now tidy it up using `(<$>)` with the `Var` constructor:

> varA2 :: Parser SimpleExpr
> varA2 = Var <$> iden
>   where
>     iden = lexemeA ((:) <$> firstChar <*> many nonFirstChar)
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

We could also split the `iden` into a separate top level function,
with the same idea as with splitting the `integer` parser.

= parens

Here is the starting point:

> parensD :: Parser SimpleExpr
> parensD = do
>     void $ lexemeA $ char '('
>     e <- simpleExprD
>     void $ lexemeA $ char ')'
>     return $ Parens e

Here is the rewrite in one step:

> parensA0 :: Parser SimpleExpr
> parensA0 =
>     Parens <$> (lexemeA (char '(')
>                 *> simpleExprD
>                 <* lexemeA (char ')'))

// f'in asciidoc, the first operator following is (*>)

Here you can see that there is a `(\*>)` which works in the opposite
direction to `(<*)`. The precendence of these operators means that we
have to use some extra parentheses (!) here.

TODO: lost the chained <*. Put something below about this so there is
a concrete example.

= simple expr

Here is the old version:

> termD :: Parser SimpleExpr
> termD = numD <|> varD <|> parensD

> simpleExprD :: Parser SimpleExpr
> simpleExprD = chainl1 termD op
>   where
>     op = do
>          void $ lexemeA $ char '+'
>          return Add

We can simplify the `op` function using the techniques we've already
seen:

> simpleExprA0 :: Parser SimpleExpr
> simpleExprA0 = chainl1 termD op
>   where op = lexemeA (char '+') *> return Add

The pattern `p *> return f` can use a different operator like this:
`f <$ p`. Here it is in the expression parser:

> simpleExprA1 :: Parser SimpleExpr
> simpleExprA1 = chainl1 termD op
>   where op = Add <$ lexemeA (char '+')

You could also write the `op` parser inline:

> simpleExprA2 :: Parser SimpleExpr
> simpleExprA2 = chainl1 termD (Add <$ lexemeA (char '+'))

Maybe this last step makes it less readable?

= summary

Here is the finished job for all the simple expression code without
separate token parsers:

> num' :: Parser SimpleExpr
> num' = (Num . read) <$> lexemeA (many1 digit)

> var' :: Parser SimpleExpr
> var' = Var <$> iden
>   where
>     iden = lexemeA ((:) <$> firstChar <*> many nonFirstChar)
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

> parens' :: Parser SimpleExpr
> parens' =
>     Parens <$> (lexemeA (char '(')
>                 *> simpleExpr'
>                 <* lexemeA (char ')'))

> term' :: Parser SimpleExpr
> term' = num' <|> var' <|> parens'

> simpleExpr' :: Parser SimpleExpr
> simpleExpr' = chainl1 term' op
>   where op = Add <$ lexemeA (char '+')

Here they are with separate token parsers and a helper function:

> lexeme :: Parser a -> Parser a
> lexeme p = p <* whitespace

> identifier :: Parser String
> identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

> integer :: Parser Integer
> integer = read <$> lexeme (many1 digit)

Here is a lexeme wrapper for parsing single character symbols.

> symbol :: Char -> Parser ()
> symbol c = void $ lexeme $ char c

Here is another little helper function. It barely pays its way in this
short example, but even though it is only used once, I think it is
worth it to make the code clearer.

> betweenParens :: Parser a -> Parser a
> betweenParens p = symbol '(' *> p <* symbol ')'

Now the expression parsers:

> num :: Parser SimpleExpr
> num = Num <$> integer

> var :: Parser SimpleExpr
> var = Var <$> identifier

> parens :: Parser SimpleExpr
> parens = Parens <$> betweenParens simpleExpr

> term :: Parser SimpleExpr
> term = num <|> var <|> parens

> simpleExpr :: Parser SimpleExpr
> simpleExpr = chainl1 term' op
>   where op = Add <$ lexemeA (char '+')

Splitting the lexer parser layer out means that we have one place
where we have to remember to add `lexeme` wrappers, and also I think
makes the code easier to follow.

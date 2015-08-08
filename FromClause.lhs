
= Overview

In this tutorial, we extend the from clause support to the following:
we will support implicit and explicit joins, including keywords
natural, inner, outer, left, right, full, cross, on and using, plus
parens and simple aliases (e.g. select a from t u, but not select a
from t(a,b)). We don't support oracle outer join syntax (+) or the
other 'pre-ANSI' variations on this theme. No lateral keyword or apply
or pivot.

> {-# LANGUAGE TupleSections #-}
>
> --import Text.Groom (groom)
> --import qualified Text.Parsec as P
> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Parsec (try)
> import Text.Parsec.String.Combinator
> import Control.Applicative ((<$>),(<*), (*>),(<*>), (<$), (<|>))
> import Control.Monad
> import Data.Maybe ()
> import qualified Test.HUnit as H
> import FunctionsAndTypesForParsing
> import Debug.Trace

TODO: should explicitly import from these two modules (and same in
QueryExpressions.lhs)

> import ValueExpressions (ValueExpr(..), valueExpr, identifier, makeTest, parens)
> import QueryExpressions (selectList,whereClause,groupByClause,having,orderBy
>                         ,commaSep1, keyword_,blackListIdentifier)

= Abstract syntax

Here are is the updated `QueryExpr` and the new `TableRef` abstract
syntax types.

> data QueryExpr
>     = Select
>       {qeSelectList :: [(ValueExpr,Maybe String)]
>       ,qeFrom :: [TableRef]
>       ,qeWhere :: Maybe ValueExpr
>       ,qeGroupBy :: [ValueExpr]
>       ,qeHaving :: Maybe ValueExpr
>       ,qeOrderBy :: [ValueExpr]
>       } deriving (Eq,Show)
>
> makeSelect :: QueryExpr
> makeSelect = Select {qeSelectList = []
>                     ,qeFrom = []
>                     ,qeWhere = Nothing
>                     ,qeGroupBy = []
>                     ,qeHaving = Nothing
>                     ,qeOrderBy = []}
>
> data TableRef = TRSimple String
>               | TRJoin TableRef JoinType TableRef (Maybe JoinCondition)
>               | TRParens TableRef
>               | TRAlias TableRef String
>               | TRQueryExpr QueryExpr
>                 deriving (Eq,Show)

This syntax for table references can represent invalid syntax, for
instance two nested aliases. The justification for this is that
sometimes trying to accurately represent only exactly what is valid
creates something much more complex. Maybe this is a good tradeoff in
this situation, and maybe not.

> data JoinType = JoinInner | JoinLeft | JoinRight | JoinFull | JoinCross
>                 deriving (Eq,Show)
>
> data JoinCondition = JoinOn ValueExpr
>                    | JoinUsing [String]
>                    | JoinNatural
>                    deriving (Eq,Show)

With the join condition, we've done the opposite to TableRef - we've
combined `natural` and `on`/`using`, since only one of these can be
present, even though this departs a little from the concrete
syntax.

First we will develop the standalone from clause parser, then we will
update the query expression syntax and parsing to incorporate our new
from clause parser.

= simple table name

Let's start with something simple: a from clause can be multiple comma
separated tablerefs, aka an implicit join.

> multipleTRSimpleTests :: [(String, [TableRef])]
> multipleTRSimpleTests = [("from a,b", [TRSimple "a", TRSimple "b"])]
>
> from0 :: Parser [TableRef]
> from0 = keyword_ "from" *> commaSep1 (TRSimple <$> identifier)

```
*Main> H.runTestTT $ H.TestList $ map (makeTest from0) multipleTRSimpleTests
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

Let's do the query expression, parens and alias first, before tackling
joins.

= subquery

Here is the example:

> trQueryExprTests :: [(String, [TableRef])]
> trQueryExprTests =
>     [("from (select a from t)"
>     ,[TRQueryExpr $ makeSelect {qeSelectList = [(Iden "a", Nothing)]
>                                ,qeFrom = [TRSimple "t"]}])]

Here is the query expression parser we can use:

> queryExpr1 :: Parser [TableRef] -> Parser QueryExpr
> queryExpr1 from' = Select
>                    <$> selectList
>                    <*> option [] from'
>                    <*> optionMaybe whereClause
>                    <*> option [] groupByClause
>                    <*> optionMaybe having
>                    <*> option [] orderBy
>
> from1 :: Parser [TableRef]
> from1 =
>     keyword_ "from" *> commaSep1 trefTerm
>   where
>     trefTerm = choice [TRSimple <$> identifier
>                       ,TRQueryExpr <$> parens (queryExpr1 from1)]

= parens

We can't do a sensible example for these right now - we need explicit
joins and then the parens can be used to override the associativity of
a three way join, or to specify over what part of the expression to
apply an alias.

> trParensTests :: [(String, [TableRef])]
> trParensTests = [("from (a)", [TRParens $ TRSimple "a"])]

We can write some more tests for parens after we've done the explicit
joins.

> from2 :: Parser [TableRef]
> from2 =
>     keyword_ "from" *> commaSep1 trefTerm
>   where
>     trefTerm = choice [TRSimple <$> identifier
>                       ,try (TRQueryExpr <$> parens (queryExpr1 from2))
>                       ,TRParens <$> parens trefTerm]

= alias

> trAliasTests :: [(String,[TableRef])]
> trAliasTests = [("from a as b", [TRAlias (TRSimple "a") "b"])
>                ,("from a b", [TRAlias (TRSimple "a") "b"])]

The alias can be treated like a postfix operator.

> suffixWrapper :: (a -> Parser a) -> a -> Parser a
> suffixWrapper p a = p a <|> return a

TODO: ?? not sure about this

> from3 :: Parser [TableRef]
> from3 =
>     keyword_ "from" *> commaSep1 trefTerm
>   where
>     trefTerm = choice [TRSimple <$> identifier
>                       ,try (TRQueryExpr <$> parens (queryExpr1 from3))
>                       ,TRParens <$> parens trefTerm]
>                >>= suffixWrapper alias
>     alias tr = TRAlias tr <$> (optional (keyword_ "as") *> identifier)

How to make it keep nesting?

= joins

Here is a casual sketch of the target grammar:

```
tref
(cross | [natural]
         ([inner]
          | left [outer]
          | right [outer]
          | full [outer]
         )
join tref
[on expr | using (...)]
```

Let's start with parsers for the 'join operator' in the middle and for
the join condition:

== join type

> joinType :: Parser JoinType
> joinType = choice
>     [JoinCross <$ keyword_ "cross" <* keyword_ "join"
>     ,JoinInner <$ keyword_ "inner" <* keyword_ "join"
>     ,JoinLeft <$ keyword_ "left"
>            <* optional (keyword_ "outer")
>            <* keyword_ "join"
>     ,JoinRight <$ keyword_ "right"
>             <* optional (keyword_ "outer")
>             <* keyword_ "join"
>     ,JoinFull <$ keyword_ "full"
>            <* optional (keyword_ "outer")
>            <* keyword_ "join"
>     ,JoinInner <$ keyword_ "join"]

```
*Main> parseWithEof joinType "cross join"
Right JoinCross

*Main> parseWithEof joinType "inner join"
Right JoinInner

*Main> parseWithEof joinType "left outer join"
Right JoinLeft

*Main> parseWithEof joinType "left join"
Right JoinLeft

*Main> parseWithEof joinType "right outer join"
Right JoinRight

*Main> parseWithEof joinType "right join"
Right JoinRight

*Main> parseWithEof joinType "full outer join"
Right JFull

*Main> parseWithEof joinType "full join"
Right JoinFull

*Main> parseWithEof joinType "join"
Right JoinInner
```

I thought about factoring out the common bits with the joinType parser:

> joinType0 :: Parser JoinType
> joinType0 = choice
>     [choice
>      [JoinCross <$ try (keyword_ "cross")
>      ,JoinInner <$ try (keyword_ "inner")
>      ,choice [JoinLeft <$ try (keyword_ "left")
>              ,JoinRight <$ try (keyword_ "right")
>              ,JoinFull <$ try (keyword_ "full")]
>       <* optional (try $ keyword_ "outer")]
>      <* keyword_ "join"
>     ,JoinInner <$ keyword_ "join"]

But I think the longer version is much easier to follow, even if it is
a little more boring.

== join condition

The idea with the join condition is that we pass a bool to say whether
we've already seen the 'natural' keyword. If so, then we don't try to
parse 'on' or 'using'.

> joinCondition :: Bool -> Parser JoinCondition
> joinCondition nat =
>     choice [guard nat >> return JoinNatural
>            ,keyword_ "on" >> JoinOn <$> valueExpr []
>            ,keyword_ "using" >> JoinUsing <$> parens (commaSep1 identifier)
>            ]

```
*Main> parseWithEof (joinCondition False) "on a"
Right (JoinOn (Iden "a"))

*Main> parseWithEof (joinCondition False) "on a + b"
Right (JoinOn (BinOp (Iden "a") "+" (Iden "b")))

*Main> parseWithEof (joinCondition False) "using (a,b)"
Right (JoinUsing ["a","b"])

*Main> parseWithEof (joinCondition True) "using (a,b)"
Left (line 1, column 1):
unexpected 'u'
expecting end of input

*Main> parseWithEof (joinCondition True) ""
Right JoinNatural
```

== simple binary join

Let's try some simple binary joins:

> simpleBinaryJoinTests :: [(String,[TableRef])]
> simpleBinaryJoinTests =
>     [("from a join b"
>      ,[TRJoin (TRSimple "a") JoinInner (TRSimple "b") Nothing])
>
>     ,("from a natural join b"
>      ,[TRJoin (TRSimple "a") JoinInner (TRSimple "b") (Just JoinNatural)])
>
>     ,("from a join b on a.x = b.y"
>      ,[TRJoin (TRSimple "a") JoinInner (TRSimple "b")
>        (Just $ JoinOn $ BinOp (DIden "a" "x") "="(DIden "b" "y"))])
>
>     ,("from a join b using(x,y)"
>      ,[TRJoin (TRSimple "a") JoinInner (TRSimple "b")
>        (Just $ JoinUsing ["x","y"])])
>
>     ,("from a cross join b"
>      ,[TRJoin (TRSimple "a") JoinCross (TRSimple "b") Nothing])
>
>     ]

We want to parse the first table, then optionally parse the 'natural'
keyword, then the join type, then the second table, then optionally
parse the join condition.

> from4 :: Parser [TableRef]
> from4 = keyword_ "from" >> (:[]) <$> do
>      t0 <- simpleTref
>      nat <- option False (True <$ keyword_ "natural")
>      jt <- joinType
>      t1 <- simpleTref
>      jc <- optionMaybe (joinCondition nat)
>      return $ TRJoin t0 jt t1 jc
>   where
>     simpleTref = TRSimple <$> identifier

Let's start extending this into the full target parser. In this next
version, I've tried to combine all the versions we've seen so far.

> from5 :: Parser [TableRef]
> from5 = keyword_ "from" >> commaSep1 tref
>   where
>     tref = nonJoinTref >>= suffixWrapper joinTrefSuffix
>     joinTrefSuffix t0 = do
>         nat <- option False (True <$ keyword_ "natural")
>         jt <- joinType
>         t1 <- nonJoinTref
>         jc <- optionMaybe (joinCondition nat)
>         return $ TRJoin t0 jt t1 jc
>     nonJoinTref = choice [TRSimple <$> identifier
>                          ,try (TRQueryExpr <$> parens (queryExpr1 from5))
>                          ,TRParens <$> parens tref]
>                   >>= suffixWrapper alias
>     alias tr = try (TRAlias tr <$> (optional (keyword_ "as") *> identifier))

```
*Main> H.runTestTT $ H.TestList $ map (makeTest from5) (multipleTRSimpleTests ++ trQueryExprTests ++ trParensTests ++ trAliasTests ++ simpleBinaryJoinTests)
 ### Failure in: 5:from a join b
(line 1, column 14):
unexpected end of input
expecting digit, letter, "_", "--" or "/*"
 ### Failure in: 6:from a natural join b
from a natural join b
expected: [TRJoin (TRSimple "a") JoinInner (TRSimple "b") (Just JoinNatural)]
 but got: [TRJoin (TRAlias (TRSimple "a") "natural") JoinInner (TRSimple "b") Nothing]
 ### Failure in: 7:from a join b on a.x = b.y
(line 1, column 15):
unexpected "o"
expecting "--" or "/*"
 ### Failure in: 8:from a join b using(x,y)
(line 1, column 15):
unexpected "u"
expecting "--" or "/*"
 ### Failure in: 9:from a cross join b
from a cross join b
expected: [TRJoin (TRSimple "a") JoinCross (TRSimple "b") Nothing]
 but got: [TRJoin (TRAlias (TRSimple "a") "cross") JoinInner (TRSimple "b") Nothing]
Cases: 10  Tried: 10  Errors: 0  Failures: 5
Counts {cases = 10, tried = 10, errors = 0, failures = 5}
```

What's going wrong? If you look at some of the issues, it looks like
we are getting keywords parsed as aliases. Let's fix that first:

> from6 :: Parser [TableRef]
> from6 = keyword_ "from" >> commaSep1 tref
>   where
>     tref = nonJoinTref >>= suffixWrapper joinTrefSuffix
>     joinTrefSuffix t0 = do
>         nat <- option False (True <$ keyword_ "natural")
>         jt <- joinType
>         t1 <- nonJoinTref
>         jc <- optionMaybe (joinCondition nat)
>         return $ TRJoin t0 jt t1 jc
>     nonJoinTref = choice [TRSimple <$> identifier
>                          ,try (TRQueryExpr <$> parens (queryExpr1 from6))
>                          ,TRParens <$> parens tref]
>                   >>= suffixWrapper alias
>     alias tr = TRAlias tr <$> (optional (keyword_ "as") *> aliasIdentifier)
>     aliasIdentifier = blackListIdentifier
>                       ["natural"
>                       ,"inner"
>                       ,"outer"
>                       ,"cross"
>                       ,"left"
>                       ,"right"
>                       ,"full"
>                       ,"join"
>                       ,"on"
>                       ,"using"]

That didn't solve the problem. I think we also have a problem since
the `alias` can now fail after consuming input, we need to use `try`.

> from7 :: Parser [TableRef]
> from7 = keyword_ "from" >> commaSep1 tref
>   where
>     tref = nonJoinTref >>= suffixWrapper joinTrefSuffix
>     joinTrefSuffix t0 = do
>         nat <- option False (True <$ keyword_ "natural")
>         jt <- joinType
>         t1 <- nonJoinTref
>         jc <- optionMaybe (joinCondition nat)
>         return $ TRJoin t0 jt t1 jc
>     nonJoinTref = choice [TRSimple <$> identifier
>                          ,try (TRQueryExpr <$> parens (queryExpr1 from7))
>                          ,TRParens <$> parens tref]
>                   >>= suffixWrapper (try . alias)
>     alias tr = try (TRAlias tr <$> (optional (keyword_ "as") *> aliasIdentifier))
>     aliasIdentifier = blackListIdentifier
>                       ["natural"
>                       ,"inner"
>                       ,"outer"
>                       ,"cross"
>                       ,"left"
>                       ,"right"
>                       ,"full"
>                       ,"join"
>                       ,"on"
>                       ,"using"]

The final step is to make it parse n-way explicit joins.

> threeWayJoinTests :: [(String,[TableRef])]
> threeWayJoinTests =
>     [("from a join b join c"
>      ,[TRJoin
>        (TRJoin (TRSimple "a") JoinInner (TRSimple "b") Nothing)
>        JoinInner (TRSimple "c") Nothing])]


> from8 :: Parser [TableRef]
> from8 = keyword_ "from" >> commaSep1 tref
>   where
>     tref = nonJoinTref >>= suffixWrapper joinTrefSuffix
>     joinTrefSuffix t0 = (do
>          nat <- option False (True <$ keyword_ "natural")
>          TRJoin t0 <$> joinType
>                    <*> nonJoinTref
>                    <*> optionMaybe (joinCondition nat))
>         >>= suffixWrapper joinTrefSuffix
>     nonJoinTref = choice [TRSimple <$> identifier
>                          ,try (TRQueryExpr <$> parens (queryExpr1 from8))
>                          ,TRParens <$> parens tref]
>                   >>= suffixWrapper (try . alias)
>     alias tr = try (TRAlias tr <$> (optional (keyword_ "as") *> aliasIdentifier))
>     aliasIdentifier = blackListIdentifier
>                       ["natural"
>                       ,"inner"
>                       ,"outer"
>                       ,"cross"
>                       ,"left"
>                       ,"right"
>                       ,"full"
>                       ,"join"
>                       ,"on"
>                       ,"using"]

We get left associative with this code. I don't know if this is correct.

We should do some more testing to make sure this code is good. TODO

= query expressions

Let's create the full query expression parser now:

> queryExprJoinTests :: [(String,QueryExpr)]
> queryExprJoinTests =
>     [("select a from t"
>      ,ms [TRSimple "t"])
>
>     ,("select a from t,u"
>      ,ms [TRSimple "t", TRSimple "u"])
>
>     ,("select a from t inner join u on expr"
>      ,ms [TRJoin (TRSimple "t") JoinInner (TRSimple "u")
>           (Just $ JoinOn $ Iden "expr")])
>
>     ,("select a from t left join u on expr"
>      ,ms [TRJoin (TRSimple "t") JoinLeft (TRSimple "u")
>           (Just $ JoinOn $ Iden "expr")])
>
>     ,("select a from t right join u on expr"
>      ,ms [TRJoin (TRSimple "t") JoinRight (TRSimple "u")
>           (Just $ JoinOn $ Iden "expr")])
>
>     ,("select a from t full join u on expr"
>      ,ms [TRJoin (TRSimple "t") JoinFull (TRSimple "u")
>           (Just $ JoinOn $ Iden "expr")])
>
>     ,("select a from t cross join u"
>      ,ms [TRJoin (TRSimple "t") JoinCross (TRSimple "u") Nothing])
>
>     ,("select a from t natural inner join u"
>      ,ms [TRJoin (TRSimple "t") JoinInner (TRSimple "u")
>           (Just JoinNatural)])
>
>     ,("select a from t inner join u using(a,b)"
>      ,ms [TRJoin (TRSimple "t") JoinInner (TRSimple "u")
>           (Just $ JoinUsing ["a", "b"])])
>
>     ,("select a from (select a from t)"
>      ,ms [TRQueryExpr $ ms [TRSimple "t"]])
>
>     ,("select a from t as u"
>      ,ms [TRAlias (TRSimple "t") "u"])
>
>     ,("select a from t u"
>      ,ms [TRAlias (TRSimple "t") "u"])
>
>     ,("select a from (t cross join u) as u"
>      ,ms [TRAlias (TRParens $ TRJoin (TRSimple "t") JoinCross
>                                      (TRSimple "u") Nothing) "u"])
>     ]
>   where
>     ms f = makeSelect {qeSelectList = [(Iden "a",Nothing)]
>                       ,qeFrom = f}

Here are all the other query expression tests updated with the new
QueryExpr type.

> singleSelectItemTests :: [(String,QueryExpr)]
> singleSelectItemTests =
>     [("select 1", makeSelect {qeSelectList = [(NumLit 1,Nothing)]})]

> multipleSelectItemsTests :: [(String,QueryExpr)]
> multipleSelectItemsTests =
>     [("select a"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)]})
>     ,("select a,b"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)
>                                  ,(Iden "b",Nothing)]})
>     ,("select 1+2,3+4"
>      ,makeSelect {qeSelectList =
>                      [(BinOp (NumLit 1) "+" (NumLit 2),Nothing)
>                      ,(BinOp (NumLit 3) "+" (NumLit 4),Nothing)]})
>     ]

> selectListTests :: [(String,QueryExpr)]
> selectListTests =
>     [("select a as a, b as b"
>      ,makeSelect {qeSelectList = [(Iden "a", Just "a")
>                                  ,(Iden "b", Just "b")]})
>
>     ,("select a a, b b"
>      ,makeSelect {qeSelectList = [(Iden "a", Just "a")
>                                  ,(Iden "b", Just "b")]})
>     ] ++ multipleSelectItemsTests
>       ++ singleSelectItemTests

> fromTests :: [(String,QueryExpr)]
> fromTests =
>     [("select a from t"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)]
>                  ,qeFrom = [TRSimple "t"]})]

> whereTests :: [(String,QueryExpr)]
> whereTests =
>     [("select a from t where a = 5"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)]
>                  ,qeFrom = [TRSimple "t"]
>                  ,qeWhere = Just $ BinOp (Iden "a") "=" (NumLit 5)})
>     ]

> groupByTests :: [(String,QueryExpr)]
> groupByTests =
>     [("select a,sum(b) from t group by a"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)
>                                  ,(App "sum" [Iden "b"],Nothing)]
>                  ,qeFrom = [TRSimple "t"]
>                  ,qeGroupBy = [Iden "a"]
>                  })
>
>     ,("select a,b,sum(c) from t group by a,b"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)
>                                  ,(Iden "b",Nothing)
>                                  ,(App "sum" [Iden "c"],Nothing)]
>                  ,qeFrom = [TRSimple "t"]
>                  ,qeGroupBy = [Iden "a",Iden "b"]
>                  })
>     ]

> havingTests :: [(String,QueryExpr)]
> havingTests =
>   [("select a,sum(b) from t group by a having sum(b) > 5"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)
>                                  ,(App "sum" [Iden "b"],Nothing)]
>                  ,qeFrom = [TRSimple "t"]
>                  ,qeGroupBy = [Iden "a"]
>                  ,qeHaving = Just $ BinOp (App "sum" [Iden "b"]) ">" (NumLit 5)
>                  })
>   ]

> orderByTests :: [(String,QueryExpr)]
> orderByTests =
>     [("select a from t order by a"
>      ,ms [Iden "a"])
>
>     ,("select a from t order by a, b"
>      ,ms [Iden "a", Iden "b"])
>     ]
>   where
>     ms o = makeSelect {qeSelectList = [(Iden "a",Nothing)]
>                       ,qeFrom = [TRSimple "t"]
>                       ,qeOrderBy = o}

```
*Main> H.runTestTT $ H.TestList $ map (makeTest (queryExpr1 from8)) (selectListTests ++ fromTests ++ whereTests ++ groupByTests ++ havingTests ++ orderByTests ++ queryExprJoinTests)
 ### Failure in: 7:select a from t where a = 5
(line 1, column 25):
unexpected "="
expecting "--" or "/*"
 ### Failure in: 8:select a,sum(b) from t group by a
(line 1, column 33):
unexpected "a"
expecting "--" or "/*"
 ### Failure in: 9:select a,b,sum(c) from t group by a,b
(line 1, column 35):
unexpected "a"
expecting "--" or "/*"
 ### Failure in: 10:select a,sum(b) from t group by a having sum(b) > 5
(line 1, column 33):
unexpected "a"
expecting "--" or "/*"
 ### Failure in: 11:select a from t order by a
(line 1, column 26):
unexpected "a"
expecting "--" or "/*"
 ### Failure in: 12:select a from t order by a, b
(line 1, column 26):
unexpected "a"
expecting "--" or "/*"
Cases: 26  Tried: 26  Errors: 0  Failures: 6
Counts {cases = 26, tried = 26, errors = 0, failures = 6}
```

The problem is the table alias parser is trying to parse keywords
again. Here is the `from` parser with the alias name blacklist
expanded.

> from :: Parser [TableRef]
> from = keyword_ "from" >> commaSep1 tref
>   where
>     tref = nonJoinTref >>= suffixWrapper joinTrefSuffix
>     joinTrefSuffix t0 = (do
>          nat <- option False (True <$ keyword_ "natural")
>          TRJoin t0 <$> joinType
>                    <*> nonJoinTref
>                    <*> optionMaybe (joinCondition nat))
>         >>= suffixWrapper joinTrefSuffix
>     nonJoinTref = choice [TRSimple <$> identifier
>                          ,try (TRQueryExpr <$> parens queryExpr)
>                          ,TRParens <$> parens tref]
>                   >>= suffixWrapper (try . alias)
>     alias tr = try (TRAlias tr <$> (optional (keyword_ "as") *> aliasIdentifier))
>     aliasIdentifier = blackListIdentifier
>                       [-- join keywords
>                        "natural"
>                       ,"inner"
>                       ,"outer"
>                       ,"cross"
>                       ,"left"
>                       ,"right"
>                       ,"full"
>                       ,"join"
>                       ,"on"
>                       ,"using"
>                        -- subsequent clause keywords
>                       ,"where"
>                       ,"group"
>                       ,"having"
>                       ,"order"
>                       ]

Here is the final query expression parser:

> queryExpr :: Parser QueryExpr
> queryExpr = Select
>             <$> selectList
>             <*> option [] from
>             <*> optionMaybe whereClause
>             <*> option [] groupByClause
>             <*> optionMaybe having
>             <*> option [] orderBy

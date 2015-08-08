
[[query-expressions]]
= Query expressions

We can now start on the 'select' parser. In the SQL standard, it
refers to these things as 'query expressions' to distinguish then from
'value expressions', so we will reuse this language here.

The subset of SQL we will support is this:

TODO: write lots of examples here

select queries only, no union, intersect or except. No common table
expressions.

we will support all the value expressions that the value expression
parser above supports

we will support select lists with optional aliases, with the 'as'
optional in the alias

we will support the * as in 'select * from t', and the variation
'select t.* from t', but not the alias version select * as (a,b,c)
from t.

we support two part dotted identifiers in value expressions, but no
other sort (such as 3-part dotted value expression identifiers, or
schema qualified table names or function names).

for the from clause, we will only support optional 'from table_name'.

supports where

we will support regular group by lists, but not the new group by
options in SQL2003 (group by (), grouping sets, cube, rollup).

we will support having

we support order by, with multiple columns, but not explicit asc or
desc , and no 'nulls first' or 'nulls last' syntax.


No support for offset and fetch first, or variations.

> {-# LANGUAGE TupleSections #-}
> module QueryExpressions where
>
> --import Text.Groom (groom)
> --import qualified Text.Parsec as P
> import Text.Parsec.String (Parser)
> import Text.Parsec (try,optionMaybe, optional, sepBy1,option)
> import Control.Applicative ((<$>),(*>),(<*>))
> import Control.Monad (void,guard)
> --import Debug.Trace
> --import Data.List (intercalate)
> import Data.Maybe ()
> import qualified Test.HUnit as H
> import FunctionsAndTypesForParsing

> import ValueExpressions (ValueExpr(..), valueExpr, identifier, symbol, keyword, comma)

Here is the datatype for query expressions to get started with. In
this tutorial, we will only support an optional single table in the
from clause, and this will be expanded in the next tutorial.

TODO: rearrange the from/where/groupby/having/orderby to a separate
datatype which is optional in a select, and the from part is mandatory
in this new type. This follows the standard and is more accurate
syntax.

> data QueryExpr
>     = Select
>       {qeSelectList :: [(ValueExpr,Maybe String)]
>       ,qeFrom :: Maybe String
>       ,qeWhere :: Maybe ValueExpr
>       ,qeGroupBy :: [ValueExpr]
>       ,qeHaving :: Maybe ValueExpr
>       ,qeOrderBy :: [ValueExpr]
>       } deriving (Eq,Show)

Here is a default value which can be used to easily construct query
expression values.

> makeSelect :: QueryExpr
> makeSelect = Select {qeSelectList = []
>                     ,qeFrom = Nothing
>                     ,qeWhere = Nothing
>                     ,qeGroupBy = []
>                     ,qeHaving = Nothing
>                     ,qeOrderBy = []}

== select lists

Let's start with something simple:

```
select [value expr]
```

TODO: shorten the names of these examples

> singleSelectItemTests :: [(String,QueryExpr)]
> singleSelectItemTests =
>     [("select 1", makeSelect {qeSelectList = [(NumLit 1,Nothing)]})]

Here are a couple of wrappers for `symbol` and `keyword` which wrap
them with void.

> keyword_ :: String -> Parser ()
> keyword_ = void . keyword

> symbol_ :: String -> Parser ()
> symbol_ = void . symbol

> singleSelectItem :: Parser QueryExpr
> singleSelectItem = do
>     keyword_ "select"
>     e <- valueExpr []
>     return $ makeSelect {qeSelectList = [(e,Nothing)]}

You can use the old test runner to check these:

```
*QueryExpressions> H.runTestTT $ H.TestList $ map (makeTest singleSelectItem) parseSingleSelectItemTestData
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

Let's rewrite it in the Applicative and mostly point free style.

> singleSelectItemApplicative :: Parser QueryExpr
> singleSelectItemApplicative =
>     (\sl -> makeSelect {qeSelectList = sl})
>     <$> (keyword_ "select" *> (((:[]) . (,Nothing)) <$> valueExpr []))

That didn't go so well. Using a wrapper:

> singleSelectItemApplicative' :: Parser QueryExpr
> singleSelectItemApplicative' =
>     ms <$> (keyword_ "select" *> valueExpr [])
>   where
>     ms e = makeSelect {qeSelectList = [(e,Nothing)]}

Now let's write something that supports multiple value expressions,
e.g.

```
select 1+2, 3+4;
```

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

> selectMultipleItems :: Parser QueryExpr
> selectMultipleItems = do
>     keyword_ "select"
>     es <- commaSep1 (valueExpr [])
>     return $ makeSelect {qeSelectList = map (,Nothing) es}

> commaSep1 :: Parser a -> Parser [a]
> commaSep1 = (`sepBy1` comma)

=== aliases

We can write names for the columns produced from a select list using
the keyword `as`, and we can miss out the `as`:

```sql
select a as a1, b as b1, f(c) as c1;

-- no as
select a a1, b b1;
```

> selectListTests :: [(String,QueryExpr)]
> selectListTests =
>     [("select a as a1, b as b1"
>      ,makeSelect {qeSelectList = [(Iden "a", Just "a1")
>                                  ,(Iden "b", Just "b1")]})
>     ,("select a a1, b b1"
>      ,makeSelect {qeSelectList = [(Iden "a", Just "a1")
>                                  ,(Iden "b", Just "b1")]})
>     ] ++ multipleSelectItemsTests
>       ++ singleSelectItemTests

Finally, here is the select list parser and the helper for select
items:

> selectItem0 :: Parser (ValueExpr, Maybe String)
> selectItem0 = (,) <$> valueExpr [] <*> optionMaybe (try alias)
>   where alias = optional (keyword_ "as") *> identifier

> selectList0 :: Parser [(ValueExpr, Maybe String)]
> selectList0 = keyword_ "select" *> commaSep1 selectItem0

> queryExpr0 :: Parser QueryExpr
> queryExpr0 = mkSelect <$> selectList0
>   where mkSelect sl = makeSelect {qeSelectList = sl}

== from clause

> from :: Parser String
> from = keyword_ "from" *> identifier

> fromTests :: [(String,QueryExpr)]
> fromTests =
>     [("select a from t"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)]
>                  ,qeFrom = (Just "t")})]

> queryExpr1 :: Parser QueryExpr
> queryExpr1 = mkSelect
>              <$> selectList0
>              <*> optionMaybe from
>   where mkSelect sl fr = makeSelect {qeSelectList = sl
>                                     ,qeFrom = fr}

```
*QueryExpressions> H.runTestTT $ H.TestList $ map (makeTest queryExpr1) (selectListTests ++ fromTests)
 ### Failure in: 6:select a from t
(line 1, column 16):
unexpected end of input
expecting digit, letter, "_", "--" or "/*"
Cases: 7  Tried: 7  Errors: 0  Failures: 1
Counts {cases = 7, tried = 7, errors = 0, failures = 1}
```

This is a keyword issue again. We are parsing the `from` as if it was
a column alias and then getting stuck.

> blackListIdentifier :: [String] -> Parser String
> blackListIdentifier bl = do
>     i <- identifier
>     guard (i `notElem` bl)
>     return i

> selectItem :: Parser (ValueExpr, Maybe String)
> selectItem = (,) <$> valueExpr [] <*> optionMaybe (try alias)
>   where alias = optional (keyword_ "as") *> blackListIdentifier ["from"]

> selectList :: Parser [(ValueExpr, Maybe String)]
> selectList = keyword_ "select" *> commaSep1 selectItem

> queryExpr2 :: Parser QueryExpr
> queryExpr2 = mkSelect
>              <$> selectList
>              <*> optionMaybe from
>   where mkSelect sl fr = makeSelect {qeSelectList = sl
>                                     ,qeFrom = fr}

That did the job for now.

== where

The where, group by, having, and order by parsers are simple.

> whereTests :: [(String,QueryExpr)]
> whereTests =
>     [("select a from t where a = 5"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)]
>                  ,qeFrom = Just "t"
>                  ,qeWhere = Just $ BinOp (Iden "a") "=" (NumLit 5)})
>     ]

> whereClause :: Parser ValueExpr
> whereClause = keyword_ "where" *> valueExpr []

> queryExpr3 :: Parser QueryExpr
> queryExpr3 = mkSelect
>              <$> selectList
>              <*> optionMaybe from
>              <*> optionMaybe whereClause
>   where mkSelect sl fr wh =
>             makeSelect {qeSelectList = sl
>                        ,qeFrom = fr
>                        ,qeWhere = wh}

== group by

> groupByTests :: [(String,QueryExpr)]
> groupByTests =
>     [("select a,sum(b) from t group by a"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)
>                                  ,(App "sum" [Iden "b"],Nothing)]
>                  ,qeFrom = Just "t"
>                  ,qeGroupBy = [Iden "a"]
>                  })
>     ,("select a,b,sum(c) from t group by a,b"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)
>                                  ,(Iden "b",Nothing)
>                                  ,(App "sum" [Iden "c"],Nothing)]
>                  ,qeFrom = Just "t"
>                  ,qeGroupBy = [Iden "a",Iden "b"]
>                  })
>     ]

> groupByClause :: Parser [ValueExpr]
> groupByClause = keyword_ "group" *> keyword_ "by"
>                 *> commaSep1 (valueExpr [])

> queryExpr4 :: Parser QueryExpr
> queryExpr4 = mkSelect
>              <$> selectList
>              <*> optionMaybe from
>              <*> optionMaybe whereClause
>              <*> option [] groupByClause
>   where mkSelect sl fr wh gr =
>             makeSelect {qeSelectList = sl
>                        ,qeFrom = fr
>                        ,qeWhere = wh
>                        ,qeGroupBy = gr}

== having

> havingTests :: [(String,QueryExpr)]
> havingTests =
>   [("select a,sum(b) from t group by a having sum(b) > 5"
>      ,makeSelect {qeSelectList = [(Iden "a",Nothing)
>                                  ,(App "sum" [Iden "b"],Nothing)]
>                  ,qeFrom = (Just "t")
>                  ,qeGroupBy = [Iden "a"]
>                  ,qeHaving = Just $ BinOp (App "sum" [Iden "b"]) ">" (NumLit 5)
>                  })
>   ]

> having :: Parser ValueExpr
> having = keyword_ "having" *> (valueExpr [])

> queryExpr5 :: Parser QueryExpr
> queryExpr5 = mkSelect
>              <$> selectList
>              <*> optionMaybe from
>              <*> optionMaybe whereClause
>              <*> option [] groupByClause
>              <*> optionMaybe having
>   where mkSelect sl fr wh gr hv =
>             makeSelect {qeSelectList = sl
>                        ,qeFrom = fr
>                        ,qeWhere = wh
>                        ,qeGroupBy = gr
>                        ,qeHaving = hv}

Looking nice so far. Did you run the tests for each stage?


== order by

> orderByTests :: [(String,QueryExpr)]
> orderByTests =
>     [("select a from t order by a"
>      ,ms [Iden "a"])
>     ,("select a from t order by a, b"
>      ,ms [Iden "a", Iden "b"])
>     ]
>   where
>     ms o = makeSelect {qeSelectList = [(Iden "a",Nothing)]
>                       ,qeFrom = (Just "t")
>                       ,qeOrderBy = o}

> orderBy :: Parser [ValueExpr]
> orderBy = keyword_ "order" *> keyword_ "by"
>           *> commaSep1 (valueExpr [])

> queryExpr6 :: Parser QueryExpr
> queryExpr6 = Select
>              <$> selectList
>              <*> optionMaybe from
>              <*> optionMaybe whereClause
>              <*> option [] groupByClause
>              <*> optionMaybe having
>              <*> option [] orderBy

```
*QueryExpressions> H.runTestTT $ H.TestList $ map (makeTest queryExpr6) (selectListTests ++ fromTests ++ whereTests ++ groupByTests ++ havingTests ++ orderByTests)
Cases: 13  Tried: 13  Errors: 0  Failures: 0
Counts {cases = 13, tried = 13, errors = 0, failures = 0}
```

TODO: talk about putting the maybes/default values inside from, where, etc??

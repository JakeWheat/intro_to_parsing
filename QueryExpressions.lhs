
We can now start on the 'select' parser. In the SQL standard, it
refers to these things as 'query expressions' to distinguish then from
'value expressions', so we will reuse this language here.

> {-# LANGUAGE TupleSections #-}
> module QueryExpressions where

TODO: qualify or add explicit imports

> --import Text.Groom (groom)
> --import qualified Text.Parsec as P
> import Text.Parsec.String (Parser)
> import Text.Parsec (try,optionMaybe, optional, sepBy1,option)
> import Control.Applicative ((<$>),(*>),(<*>))
> --import Control.Monad (void,guard)
> --import Debug.Trace
> --import Data.List (intercalate)
> import Data.Maybe ()
> --import qualified Test.HUnit as H

> import ValueExpressions

Here is the datatype for query expressions to get started with. In
this tutorial, we will only support an optional single table in the
from clause, and this will be expanded in the next tutorial.

> data QueryExpr
>     = Select
>       {qeSelectList :: [(ValueExpr,Maybe String)]
>       ,qeFrom :: [TableRef]
>       ,qeWhere :: Maybe ValueExpr
>       ,qeGroupBy :: [ValueExpr]
>       ,qeHaving :: Maybe ValueExpr
>       ,qeOrderBy :: [ValueExpr]
>       } deriving (Eq,Show)

Here are the tableref types:

> data TableRef = SimpleTableRef String
>               | JoinTableRef JoinType TableRef TableRef (Maybe JoinCondition)
>               | JoinParens TableRef
>               | JoinAlias TableRef String
>               | JoinQueryExpr QueryExpr
>                 deriving (Eq,Show)

> data JoinType = Inner | JLeft | JRight | Full | Cross
>                 deriving (Eq,Show)

> data JoinCondition = JoinOn ValueExpr
>                    | JoinUsing [String]
>                    | JoinNatural
>                 deriving (Eq,Show)


The subset of SQL we will support is this:

queries only, no explicit table 'table t', no table value constructor
'values (1,2)', no union, intersect or except.

we will support all the value expressions that the value expression
parser above supports

we will support select lists with optional aliases, with the 'as'
optional in the alias

we will support the * as in 'select * from t', and the variation
'select t.* from t', but not the alias version select * as (a,b,c)
from t.

we support two part dotted identifiers in value expressions, but no
other sort (such as 3-part dotted value expression identifiers, or
schema qualified table names).

we will support regular group by lists, but not the new group by
options in SQL2003 (group by (), grouping sets, cube, rollup).

we will support having

we support order by, with multiple columns, but not explicit asc or
desc , and no 'nulls first' or 'nulls last' syntax.

For the from clause, we will support implicit and explicit joins,
including keywords natural, inner, outer, left, right, full, cross, on
and using, plus parens and simple aliases (e.g. select a from t u, but
not select a from t(a,b)). We don't support oracle outer join syntax
(+) or the other 'pre-ANSI' variations on this theme. No lateral
keyword or apply or pivot. All of this will be in the next
tutorial. In this tutorial, we only support optional 'from
table_name'.

No support for offset and fetch first, or variations.

Each constructor will be explained when we come to it. The tableref
type is below in the 'from' section.

Here is a default value which can be used to easily construct query
expression values.

> makeSelect :: QueryExpr
> makeSelect = Select {qeSelectList = []
>                     ,qeFrom = []
>                     ,qeWhere = Nothing
>                     ,qeGroupBy = []
>                     ,qeHaving = Nothing
>                     ,qeOrderBy = []}

= select lists

Let's start with something simple:

select [value expr]

> parseSingleSelectItemTestData :: [(String,QueryExpr)]
> parseSingleSelectItemTestData =
>     [("select 1", makeSelect {qeSelectList = [(NumberLiteral 1,Nothing)]})]

> singleSelectItem :: Parser QueryExpr
> singleSelectItem = do
>     keyword_ "select"
>     e <- valueExpr
>     return $ makeSelect {qeSelectList = [(e,Nothing)]}

Here is an example where rewriting to use applicative can make the
parser code much less clear:

> singleSelectItemApplicative :: Parser QueryExpr
> singleSelectItemApplicative =
>     (\sl -> makeSelect {qeSelectList = sl})
>     <$> (keyword_ "select" *> (((:[]) . (,Nothing)) <$> valueExpr))

Using a helper function can make this version more readable:

> singleSelectItemApplicative' :: Parser QueryExpr
> singleSelectItemApplicative' =
>     ms <$> (keyword_ "select" *> valueExpr)
>   where
>     ms e = makeSelect {qeSelectList = [(e,Nothing)]}

Now we can write something that supports multiple value expressions,
e.g.

select 1+2, 3+4;


> parseMultipleSelectItemsTestData :: [(String,QueryExpr)]
> parseMultipleSelectItemsTestData =
>     [("select a"
>      ,makeSelect {qeSelectList = [(Identifier "a",Nothing)]})
>     ,("select a,b"
>      ,makeSelect {qeSelectList = [(Identifier "a",Nothing)
>                                  ,(Identifier "b",Nothing)]})
>     ,("select 1+2,3+4"
>      ,makeSelect {qeSelectList =
>                      [(BinaryOp (NumberLiteral 1) "+" (NumberLiteral 2),Nothing)
>                      ,(BinaryOp (NumberLiteral 3) "+" (NumberLiteral 4),Nothing)]})
>     ]


> selectMultipleItems :: Parser QueryExpr
> selectMultipleItems = do
>     keyword_ "select"
>     es <- commaSep1 valueExpr
>     return $ makeSelect {qeSelectList = map (,Nothing) es}

Here is another helper parser which will be used a lot.

> commaSep1 :: Parser a -> Parser [a]
> commaSep1 = (`sepBy1` symbol_ ",")

== aliases

We can write names for the columns produced from a select list using
the keyword 'as', and we can miss out the as:

select a as a, b as b, f(c) c;

select a a, b b;

> parseSelectListTestData :: [(String,QueryExpr)]
> parseSelectListTestData =
>     [("select a as a, /*comment*/ b as b"
>      ,makeSelect {qeSelectList = [(Identifier "a", Just "a")
>                                  ,(Identifier "b", Just "b")]})
>     ,("select a a, b b"
>      ,makeSelect {qeSelectList = [(Identifier "a", Just "a")
>                                  ,(Identifier "b", Just "b")]})
>     ]

fixing the issue with 'as'/ 'a': demonstrate the issue with the non
blacklist identifier parser


Finally, here is the select list parser and the helper for select
items:

> selectItem :: Parser (ValueExpr, Maybe String)
> selectItem = (,) <$> valueExpr <*> optionMaybe (try alias)
>   where alias = optional (try (keyword_ "as")) *> identifierString

TODO: note about optional in parsec and in applicative


> selectList :: Parser [(ValueExpr, Maybe String)]
> selectList = try (keyword_ "select") *> commaSep1 selectItem


= simplified from clause

> from :: Parser [TableRef]
> from = option [] (try (keyword_ "from") *> (mkFrom <$> identifierString))
>   where mkFrom f = [SimpleTableRef f]

> parseFromTestData :: [(String,QueryExpr)]
> parseFromTestData =
>     [("select a from t"
>      ,makeSelect {qeSelectList = [(Identifier "a",Nothing)]
>                  ,qeFrom = [SimpleTableRef "t"]})]

= where

The where, group by, having, and order by parsers are simple.

> parseWhereTestData :: [(String,QueryExpr)]
> parseWhereTestData =
>     [("select a from t where a = 5"
>      ,makeSelect {qeSelectList = [(Identifier "a",Nothing)]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeWhere = Just $ BinaryOp (Identifier "a") "=" (NumberLiteral 5)})
>     ]

> swhere :: Parser (Maybe ValueExpr)
> swhere = optionMaybe (try (keyword_ "where") *> valueExpr)

= group by

> parseGroupByTestData :: [(String,QueryExpr)]
> parseGroupByTestData =
>     [("select a,sum(b) from t group by a"
>      ,makeSelect {qeSelectList = [(Identifier "a",Nothing)
>                                  ,(App "sum" [Identifier "b"],Nothing)]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeGroupBy = [Identifier "a"]
>                  })
>     ,("select a,b,sum(c) from t group by a,b"
>      ,makeSelect {qeSelectList = [(Identifier "a",Nothing)
>                                  ,(Identifier "b",Nothing)
>                                  ,(App "sum" [Identifier "c"],Nothing)]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeGroupBy = [Identifier "a",Identifier "b"]
>                  })
>     ]

> sgroupBy :: Parser [ValueExpr]
> sgroupBy = option [] (try (keyword_ "group")
>                       *> keyword_ "by"
>                       *> commaSep1 valueExpr)

= having

> parseHavingTestData :: [(String,QueryExpr)]
> parseHavingTestData =
>   [("select a,sum(b) from t group by a having sum(b) > 5"
>      ,makeSelect {qeSelectList = [(Identifier "a",Nothing)
>                                  ,(App "sum" [Identifier "b"],Nothing)]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeGroupBy = [Identifier "a"]
>                  ,qeHaving = Just $ BinaryOp (App "sum" [Identifier "b"]) ">" (NumberLiteral 5)
>                  })
>   ]

> having :: Parser (Maybe ValueExpr)
> having = optionMaybe (try (keyword_ "having") *> valueExpr)

= order by

> parseOrderByTestData :: [(String,QueryExpr)]
> parseOrderByTestData =
>     [("select a from t order by a"
>      ,ms [Identifier "a"])
>     ,("select a from t order by a, b"
>      ,ms [Identifier "a", Identifier "b"])
>     ]
>   where
>     ms o = makeSelect {qeSelectList = [(Identifier "a",Nothing)]
>                       ,qeFrom = [SimpleTableRef "t"]
>                       ,qeOrderBy = o}

> orderBy :: Parser [ValueExpr]
> orderBy = option [] (try (keyword_ "order")
>                      *> keyword_ "by"
>                      *> commaSep1 valueExpr)

= putting together the query expression parser

> queryExpr :: Parser QueryExpr
> queryExpr =
>     Select
>     <$> selectList
>     <*> from
>     <*> swhere
>     <*> sgroupBy
>     <*> having
>     <*> orderBy

Here are all the query expression parsing tests put together

> parseQueryExprTestData :: [(String,QueryExpr)]
> parseQueryExprTestData =
>     concat [parseSingleSelectItemTestData
>            ,parseMultipleSelectItemsTestData
>            ,parseSelectListTestData
>            ,parseFromTestData
>            ,parseWhereTestData
>            ,parseGroupByTestData
>            ,parseHavingTestData
>            ,parseOrderByTestData]

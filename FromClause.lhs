
In this tutorial, we extend the from clause support to the following:
we will support implicit and explicit joins, including keywords
natural, inner, outer, left, right, full, cross, on and using, plus
parens and simple aliases (e.g. select a from t u, but not select a
from t(a,b)). We don't support oracle outer join syntax (+) or the
other 'pre-ANSI' variations on this theme. No lateral keyword or apply
or pivot.

TODO: redo this all from scratch.

> {-# LANGUAGE TupleSections #-}
>
> --import Text.Groom (groom)
> --import qualified Text.Parsec as P
> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Parsec (try)
> import Text.Parsec.String.Combinator (optional, option,choice)
> import Control.Applicative ((<$>),(*>),(<*>), (<$))
> --import Control.Monad (void,guard)
> --import Debug.Trace
> --import Data.List (intercalate)
> import Data.Maybe ()
> --import qualified Test.HUnit as H

> import ValueExpressions
> import QueryExpressions hiding (from)


```
data TableRef = SimpleTableRef String
              | JoinTableRef JoinType TableRef TableRef (Maybe JoinCondition)
              | JoinParens TableRef
              | JoinAlias TableRef String
              | JoinQueryExpr QueryExpr
                deriving (Eq,Show)

data JoinType = Inner | JLeft | JRight | Full | Cross
                deriving (Eq,Show)

data JoinCondition = JoinOn ValueExpr
                   | JoinUsing [String]
                   | JoinNatural
                deriving (Eq,Show)
```

rough grammar

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


Here are the examples:

> parseFromTestData :: [(String,QueryExpr)]
> parseFromTestData =
>     [("select a from t"
>      ,ms [SimpleTableRef "t"])
>     ,("select a from t,u"
>      ,ms [SimpleTableRef "t", SimpleTableRef "u"])
>     ,("select a from t inner join u on expr"
>      ,ms [JoinTableRef Inner (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Identifier "expr")])
>     ,("select a from t left join u on expr"
>      ,ms [JoinTableRef JLeft (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Identifier "expr")])
>     ,("select a from t right join u on expr"
>      ,ms [JoinTableRef JRight (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Identifier "expr")])
>     ,("select a from t full join u on expr"
>      ,ms [JoinTableRef Full (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Identifier "expr")])
>     ,("select a from t cross join u"
>      ,ms [JoinTableRef Cross (SimpleTableRef "t")
>                             (SimpleTableRef "u") Nothing])
>     ,("select a from t natural inner join u"
>      ,ms [JoinTableRef Inner (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just JoinNatural)])
>     ,("select a from t inner join u using(a,b)"
>      ,ms [JoinTableRef Inner (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinUsing ["a", "b"])])
>     ,("select a from (select a from t)"
>      ,ms [JoinQueryExpr $ ms [SimpleTableRef "t"]])
>     ,("select a from t as u"
>      ,ms [JoinAlias (SimpleTableRef "t") "u"])
>     ,("select a from t u"
>      ,ms [JoinAlias (SimpleTableRef "t") "u"])
>     ,("select a from (t cross join u) as u"
>      ,ms [JoinAlias (JoinParens $ JoinTableRef Cross (SimpleTableRef "t")
>                             (SimpleTableRef "u") Nothing) "u"])
>     ]
>   where
>     ms f = makeSelect {qeSelectList = [(Identifier "a",Nothing)]
>                       ,qeFrom = f}

TODO: spend a lot of time building up the from parser with different
variations and prototypes.

The final from is quite a complicated looking query. A lot of that is
because of all the keywords.

TODO: try and simplify this code some more.

> from :: Parser [TableRef]
> from = try (keyword_ "from") *> commaSep1 tref
>   where
>     tref = nonJoinTref >>= optionSuffix joinTrefSuffix
>     nonJoinTref = choice [try (TRQueryExpr <$> parens queryExpr)
>                          ,TRParens <$> parens tref
>                          ,TRLateral <$> (try (keyword_ "lateral")
>                                          *> nonJoinTref)
>                          ,try (TRFunction <$> name
>                                           <*> parens (commaSep valueExpr))
>                          ,TRSimple <$> name]
>                   >>= optionSuffix aliasSuffix
>     aliasSuffix j = option j (TRAlias j <$> alias)
>     joinTrefSuffix t = (do
>          nat <- option False $ try (True <$ try (keyword_ "natural"))
>          TRJoin t <$> joinType
>                   <*> nonJoinTref
>                   <*> optionMaybe (joinCondition nat))
>         >>= optionSuffix joinTrefSuffix
>     joinType =
>         choice [choice
>                 [JCross <$ try (keyword_ "cross")
>                 ,JInner <$ try (keyword_ "inner")
>                 ,choice [JLeft <$ try (keyword_ "left")
>                         ,JRight <$ try (keyword_ "right")
>                         ,JFull <$ try (keyword_ "full")]
>                  <* optional (try $ keyword_ "outer")]
>                 <* keyword "join"
>                ,JInner <$ keyword_ "join"]
>     joinCondition nat =
>         choice [guard nat >> return JoinNatural
>                ,try (keyword_ "on") >>
>                 JoinOn <$> valueExpr
>                ,try (keyword_ "using") >>
>                 JoinUsing <$> parens (commaSep1 name)
>                ]

> alias :: Parser Alias
> alias = Alias <$> try tableAlias <*> try columnAliases
>   where
>     tableAlias = optional (try $ keyword_ "as") *> name
>     columnAliases = optionMaybe $ try $ parens $ commaSep1 name

Here is another small helper parser. Having the arguments in this
order makes it easy to chain using >>=.

> optionSuffix :: (a -> Parser a) -> a -> Parser a
> optionSuffix p a = option a (p a)

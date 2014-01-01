
In this tutorial, we will cover parsing the from clause in SQL, which
is quite complex on its own.

TODO: redo this all from scratch.

> {-# LANGUAGE TupleSections #-}

TODO: qualify or add explicit imports

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


~~~~
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
~~~~

rough grammar

~~~~
tref
(cross | [natural]
         ([inner]
          | left [outer]
          | right [outer]
          | full [outer]
         )
join tref
[on expr | using (...)]
~~~~


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
> from = option [] (try (keyword_ "from") *> commaSep1 tref)
>   where
>     tref = choice [try (JoinQueryExpr <$> parens queryExpr)
>                   ,JoinParens <$> parens tref
>                   ,SimpleTableRef <$> identifierString]
>            >>= optionSuffix join
>            >>= optionSuffix alias
>     join tref0 =
>         choice
>         [try (keyword_ "natural") *> keyword_ "inner"
>          *> conditionlessSuffix tref0 Inner (Just JoinNatural)
>         ,try (keyword_ "join")
>          *> (JoinTableRef Inner tref0 <$> tref <*> joinExpr)
>         ,try (keyword_ "inner")
>          *> conditionSuffix tref0 Inner
>         ,try (choice [JLeft <$ keyword_ "left"
>                      ,JRight <$ keyword_ "right"
>                      ,Full <$ keyword_ "full"])
>          >>= outerJoinSuffix tref0
>         ,try (keyword_ "cross")
>          *> conditionlessSuffix tref0 Cross Nothing
>         ]
>         >>= optionSuffix join
>     outerJoinSuffix tref0 jt =
>         optional (keyword_ "outer") *> conditionSuffix tref0 jt
>     conditionSuffix tref0 jt =
>         keyword_ "join" *> (JoinTableRef jt tref0 <$> tref <*> joinExpr)
>     conditionlessSuffix tref0 jt jc =
>         keyword_ "join" *> (JoinTableRef jt tref0 <$> tref <*> return jc)
>     joinExpr = choice
>                [(Just . JoinUsing)
>                  <$> (try (keyword_ "using")
>                       *> parens (commaSep1 identifierString))
>                ,(Just . JoinOn) <$> (try (keyword_ "on") *> valueExpr)
>                ,return Nothing
>                ]
>     alias j = let a1 = optional (try (keyword_ "as")) *> identifierString
>               in option j (JoinAlias j <$> try a1)

Here is another small helper parser. Having the arguments in this
order makes it easy to chain using >>=.

> optionSuffix :: (a -> Parser a) -> a -> Parser a
> optionSuffix p a = option a (p a)

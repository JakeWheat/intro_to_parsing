
Here is a pretty printer for the parser version 0.

It uses Text.PrettyPrint module. I haven't put much
comments/explanation here.

> module PrettyPrinting0 where


> import SimpleSQLQueryParser0 (ValueExpr(..), QueryExpr(..), TableRef(..)
>                              ,JoinType(..), JoinCondition(..))
> import qualified SimpleSQLQueryParser0 as S
> import Text.PrettyPrint (render, vcat, text, (<>), (<+>), empty, parens,
>                          nest, Doc, punctuate, comma, sep, quotes,
>                          doubleQuotes, hsep)
> import Data.Maybe (maybeToList, catMaybes)
> import FunctionsAndTypesForParsing
> --import Text.Parsec.String.Combinator (eof)
> import qualified Test.HUnit as H
> import Text.Parsec (parse, ParseError)
> import Control.Applicative ((<$>),(<*), (*>),(<*>), (<$), (<|>), many)
> --import Text.Parsec.String (Parser)

The basic concept in this pretty printer is we convert the ast into
`Doc` values, then convert these into a string. We have a bunch of
functions to convert `String` to `Doc`s, and combine these docs with
different layouts.

By using a pretty printer library, we can get human readable source
very easily compared with trying to convert directly to strings
ourselves.

= api

> prettyQueryExpr :: QueryExpr -> String
> prettyQueryExpr = render . queryExpr

= value expressions

> valueExpr :: ValueExpr -> Doc
> valueExpr (StringLit s) = quotes $ text s
> valueExpr (NumLit i) = text $ show i
> valueExpr (Iden s) = text s
> valueExpr (DIden q i) = text q <> text "." <> text i
> valueExpr Star = text "*"
> valueExpr (DStar q) = text q <> text "." <> text "*"
> valueExpr (App f es) = text f <> parens (commaSep $ map valueExpr es)
> valueExpr (PrefOp op e) = sep[text op, valueExpr e]
> valueExpr (BinOp e0 op e1) = sep [valueExpr e0, text op, valueExpr e1]
> valueExpr (Case test whens els) =
>     sep [text "case" <+> maybe empty valueExpr test
>         ,nest 5 $ sep (map wh whens
>                        ++ [maybe empty
>                            (\e -> text "else" <+> valueExpr e)
>                            els])
>         ,text "end"]
>   where wh (w,t) = sep [text "when" <+> valueExpr w
>                        ,text "then" <+> valueExpr t]
> valueExpr (Parens e) = parens $ valueExpr e

= query expressions

> queryExpr :: QueryExpr -> Doc
> queryExpr (Select sl fr wh gb hv ob) = sep
>     [text "select" <+> nest 7 (commaSep $ map selectItem sl)
>      -- from
>     ,ml fr $ \f -> text "from" <+> nest 7 (commaSep $ map tref f)
>      -- where
>     ,me wh $ \w -> text "where" <+> nest 6 (valueExpr w)
>      -- group by
>     ,ml gb $ \g -> text "group by" <+> nest 9 (commaSep $ map valueExpr g)
>      -- having
>     ,me hv $ \h -> text "having" <+> nest 6 (valueExpr h)
>      -- order by
>     ,ml ob $ \o -> text "order by" <+> nest 9 (commaSep $ map valueExpr o)
>     ]
>   where
>     selectItem (e,a) = valueExpr e <+> me a (\a' -> text "as" <+> text a')
>
>     tref (TRSimple t) = text t
>     tref (TRParens t) = parens $ tref t
>     tref (TRAlias t a) = tref t <+> text "as" <+> text a
>     tref (TRQueryExpr q) = parens $ queryExpr q
>     tref (TRJoin t0 jt t1 jc) = sep
>         [tref t0
>         ,joinName jt jc <+> tref t1
>         ,case jc of
>              Just (JoinOn e) -> text "on" <+> valueExpr e
>              Just (JoinUsing is) -> text "using" <+> parens (commaSep $ map text is)
>              Just JoinNatural -> empty
>              Nothing -> empty]
>     joinName jt jc =
>       hsep [case jc of
>                Just JoinNatural -> text "natural"
>                _ -> empty
>            ,case jt of
>                JoinInner -> text "inner join"
>                JoinCross -> text "cross join"
>                JoinLeft -> text "left join"
>                JoinRight -> text "right join"
>                JoinFull -> text "full join"]
>     me e r = maybe empty r e
>     ml [] _ = empty
>     ml l r = r l

= helpers

> commaSep :: [Doc] -> Doc
> commaSep = sep . punctuate comma

Have a look at the haddock for this module and see if you can work out
how the code above works.

```
*PrettyPrinting0> either (error . show) valueExpr (parseWithEof (S.valueExpr []) "a and b")
a and b

*PrettyPrinting0> either (error . show) queryExpr (parseWithEof S.queryExpr "select a from t inner join u using(a,b)")
select a from t inner join u using (a, b)
```

= tests

Now we can do some tests: we take the previous test data, and for each
test add an additional test which pretty prints then parses the
results to see that it is unchanged.

> makeTest :: (Eq a, Show a) =>
>             (String -> Either ParseError a)
>          -> (a -> String)
>          -> (String,a)
>          -> H.Test
> makeTest parser pretty (src,expected) = H.TestLabel src $ H.TestCase $ do
>     let gote = parser src
>     case gote of
>       Left e -> H.assertFailure $ show e
>       Right got -> do
>         H.assertEqual src expected got
>         let prsql = pretty got
>             gotpretty = parser prsql
>         case gotpretty of
>           Left e -> H.assertFailure $ "pretty: " ++ prsql ++ "\n" ++ show e
>           Right gotp -> H.assertEqual ("pretty: " ++ prsql) expected gotp

TODO: fix parsing issue

```
*PrettyPrinting0> H.runTestTT $ H.TestList $ map (makeTest S.parseQueryExpr (render . queryExpr)) S.allQueryExprTests
Cases: 26  Tried: 26  Errors: 0  Failures: 0
Counts {cases = 26, tried = 26, errors = 0, failures = 0}
*PrettyPrinting0> H.runTestTT $ H.TestList $ map (makeTest S.parseValueExpr (render . valueExpr)) S.allValueExprTests
 ### Failure in: 34:case when a=1 then 2 when a=3 then 4 else 5 end
(line 1, column 11):
unexpected "a"
expecting "--" or "/*"
Cases: 35  Tried: 35  Errors: 0  Failures: 1
Counts {cases = 35, tried = 35, errors = 0, failures = 1}
```

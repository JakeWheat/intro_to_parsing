
Here is the complete syntax, parser and tests for the value and query
expressions so far as a self contained module

> module SimpleSQLQueryParser0 where
>
> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Parsec (try)
> import Text.Parsec.String.Char
> import Text.Parsec.String.Combinator
> import Text.Parsec (parse)
> import Control.Applicative ((<$>),(<*), (*>),(<*>), (<$), (<|>), many)
> import qualified Text.Parsec.String.Expr as E
> import Control.Monad
> --import Data.List (intercalate)
> import Data.Maybe ()
> import qualified Test.HUnit as H
> import FunctionsAndTypesForParsing
> import Debug.Trace

= Supported SQL

== comments

```sql
-- single line comment
/*
multiline
comment
*/
```

The `/* */` comments do not nest.

== value expressions

=== literals

postive integral literals and string literals with single quote,
without escaping of single quote within string (so there is no way to
create a string literal with a single quote in it).

```sql
1
500
'string literal'
```
=== identifiers

Unquoted identifiers only, an identifier may start with a letter or
underscore, and contain letters, underscores and digits.

```sql
a
something
_test_
a123
```

=== dotted identifiers

Supports two part dotted identifiers only. Both parts must parse
according to the rules for regular identifiers.

```sql
t.a
something.something_else
```

=== star

Star, plus dotted star using the identifier rules for the first part.

```sql
*
t.*
```

=== function application

The function name must parse as a valid identifier.

```sql
f()
g(1)
h(2,'something')
```

=== operators

Here is the range of operators supported.

```sql
a = b
a > b
a < b
a >= b
a <= b
a != b
a <> b
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

=== case expression

```sql
case a
when 3 then 'got three'
when 5 then 'got five'
else 'neither'
end
```

```sql
case
when a = 3 then 'a is three'
when b = 4 then 'b is four'
else 'neither'
end
```

The else branch is optional in both cases.

=== parentheses

```sql
(1 + 2) * 3
```

Parentheses are explicit in the abstract syntax.

== query expressions

TODO: examples

select queries only, no union, intersect or except. No common table
expressions.

select list aliases, with the 'as' optional in the alias

'select * from t'
'select t.* from t'
but not the alias version 'select * as (a,b,c) from t'.

from clause

implicit and explicit joins, including keywords
natural, inner, outer, left, right, full, cross, on and using, plus
parens and simple aliases (e.g. select a from t u, but not select a
from t(a,b)).

where

group by lists
but not the new group by options in SQL2003 (group by (), grouping
sets, cube, rollup).

having

order by, with multiple columns, but not explicit asc or
desc , and no 'nulls first' or 'nulls last' syntax.

No support for offset and fetch first, or variations.


= Abstract syntax

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

> data QueryExpr
>     = Select
>       {qeSelectList :: [(ValueExpr,Maybe String)]
>       ,qeFrom :: [TableRef]
>       ,qeWhere :: Maybe ValueExpr
>       ,qeGroupBy :: [ValueExpr]
>       ,qeHaving :: Maybe ValueExpr
>       ,qeOrderBy :: [ValueExpr]
>       } deriving (Eq,Show)

> makeSelect :: QueryExpr
> makeSelect = Select {qeSelectList = []
>                     ,qeFrom = []
>                     ,qeWhere = Nothing
>                     ,qeGroupBy = []
>                     ,qeHaving = Nothing
>                     ,qeOrderBy = []}

> data TableRef = TRSimple String
>               | TRJoin TableRef JoinType TableRef (Maybe JoinCondition)
>               | TRParens TableRef
>               | TRAlias TableRef String
>               | TRQueryExpr QueryExpr
>                 deriving (Eq,Show)

> data JoinType = JoinInner | JoinLeft | JoinRight | JoinFull | JoinCross
>                 deriving (Eq,Show)

> data JoinCondition = JoinOn ValueExpr
>                    | JoinUsing [String]
>                    | JoinNatural
>                    deriving (Eq,Show)

= Value expression parsing

== term components

> num :: Parser ValueExpr
> num = NumLit <$> integer

> iden :: [String] -> Parser ValueExpr
> iden blacklist = Iden <$> identifierBlacklist blacklist

> parensValue :: Parser ValueExpr
> parensValue = Parens <$> parens (valueExpr [])

> stringLit :: Parser ValueExpr
> stringLit = StringLit <$> stringToken

> dIden :: Parser ValueExpr
> dIden = DIden <$> identifier <*> (dot *> identifier)

> star :: Parser ValueExpr
> star = Star <$ symbol "*"

> dstar :: Parser ValueExpr
> dstar = DStar <$> (identifier <* dot <* symbol "*")

> app :: Parser ValueExpr
> app = App <$> identifier <*> parens (commaSep $ valueExpr [])

== case

> caseValue :: Parser ValueExpr
> caseValue =
>     Case
>     <$> (keyword "case" *> optionMaybe caseVal)
>     <*> many1 whenClause
>     <*> optionMaybe elseClause
>     <* keyword "end"
>  where
>    whenClause = (,) <$> (keyword "when" *> caseVal)
>                     <*> (keyword "then" *> caseVal)
>    elseClause = keyword "else" *> caseVal
>    caseVal = valueExpr blackList
>    blackList = ["case", "when", "then", "else", "end"]

== term

> term :: [String] -> Parser ValueExpr
> term blackList = choice [caseValue
>                         ,try app
>                         ,try dstar
>                         ,try dIden
>                         ,iden blackList
>                         ,num
>                         ,parensValue
>                         ,stringLit
>                         ,star]

== operators

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

== valueExpr

> valueExpr :: [String] -> Parser ValueExpr
> valueExpr blackList = E.buildExpressionParser table (term blackList)

= Query expression parsing

> selectList :: Parser [(ValueExpr, Maybe String)]
> selectList = keyword_ "select" *> commaSep1 selectItem

> selectItem :: Parser (ValueExpr, Maybe String)
> selectItem = (,) <$> valueExpr [] <*> optionMaybe (try alias)
>   where alias = optional (keyword_ "as") *> identifierBlacklist ["from"]

> whereClause :: Parser ValueExpr
> whereClause = keyword_ "where" *> valueExpr []

> groupByClause :: Parser [ValueExpr]
> groupByClause = keyword_ "group" *> keyword_ "by"
>                 *> commaSep1 (valueExpr [])

> having :: Parser ValueExpr
> having = keyword_ "having" *> (valueExpr [])

> orderBy :: Parser [ValueExpr]
> orderBy = keyword_ "order" *> keyword_ "by"
>           *> commaSep1 (valueExpr [])

== from clause

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
>     aliasIdentifier = identifierBlacklist
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

> joinCondition :: Bool -> Parser JoinCondition
> joinCondition nat =
>     choice [guard nat >> return JoinNatural
>            ,keyword_ "on" >> JoinOn <$> valueExpr []
>            ,keyword_ "using" >> JoinUsing <$> parens (commaSep1 identifier)
>            ]

== queryExpr

> queryExpr :: Parser QueryExpr
> queryExpr = Select
>             <$> selectList
>             <*> option [] from
>             <*> optionMaybe whereClause
>             <*> option [] groupByClause
>             <*> optionMaybe having
>             <*> option [] orderBy

= tokens

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

> stringToken :: Parser String
> stringToken = lexeme (char '\'' *> manyTill anyChar (char '\''))

> dot :: Parser Char
> dot = lexeme $ char '.'

> comma :: Parser Char
> comma = lexeme $ char ','

= helper functions

> keyword :: String -> Parser String
> keyword k = try $ do
>     i <- identifier
>     guard (i == k)
>     return k

> parens :: Parser a -> Parser a
> parens = between openParen closeParen

> commaSep :: Parser a -> Parser [a]
> commaSep = (`sepBy` comma)

> keyword_ :: String -> Parser ()
> keyword_ = void . keyword

> symbol_ :: String -> Parser ()
> symbol_ = void . symbol

> commaSep1 :: Parser a -> Parser [a]
> commaSep1 = (`sepBy1` comma)

> identifierBlacklist :: [String] -> Parser String
> identifierBlacklist bl = do
>     i <- identifier
>     guard (i `notElem` bl)
>     return i

> suffixWrapper :: (a -> Parser a) -> a -> Parser a
> suffixWrapper p a = p a <|> return a

= tests

> data TestItem = Group String [TestItem]
>               | ValueExpressionTest String ValueExpr
>               | QueryExpressionTest String QueryExpr

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

> stringLiteralTests :: [(String,ValueExpr)]
> stringLiteralTests =
>     [("''", StringLit "")
>     ,("'test'", StringLit "test")]

> dIdenTests :: [(String,ValueExpr)]
> dIdenTests =
>     [("t.a", DIden "t" "a")]

> starTests :: [(String,ValueExpr)]
> starTests = [("*", Star)]


> dStarTests :: [(String,ValueExpr)]
> dStarTests = [("t.*", DStar "t")]

> appTests :: [(String,ValueExpr)]
> appTests = [("f()", App "f" [])
>            ,("f(1)", App "f" [NumLit 1])
>            ,("f(1,a)", App "f" [NumLit 1, Iden "a"])]

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

> allValueExprTests :: [(String,ValueExpr)]
> allValueExprTests = concat [basicTests
>                             ,stringLiteralTests
>                             ,dIdenTests
>                             ,starTests
>                             ,dStarTests
>                             ,appTests
>                             ,caseTests]

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
>     ,("select a from t order by a, b"
>      ,ms [Iden "a", Iden "b"])
>     ]
>   where
>     ms o = makeSelect {qeSelectList = [(Iden "a",Nothing)]
>                       ,qeFrom = [TRSimple "t"]
>                       ,qeOrderBy = o}

> queryExprJoinTests :: [(String,QueryExpr)]
> queryExprJoinTests =
>     [("select a from t"
>      ,ms [TRSimple "t"])
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

> allQueryExprTests :: [(String,QueryExpr)]
> allQueryExprTests = concat [selectListTests ++ fromTests ++ whereTests ++ groupByTests ++ havingTests ++ orderByTests ++ queryExprJoinTests]

> makeTest :: (Eq a, Show a) => Parser a -> (String,a) -> H.Test
> makeTest parser (src,expected) = H.TestLabel src $ H.TestCase $ do
>     let gote = parse (whitespace *> parser <* eof) "" src
>     case gote of
>       Left e -> H.assertFailure $ show e
>       Right got -> H.assertEqual src expected got


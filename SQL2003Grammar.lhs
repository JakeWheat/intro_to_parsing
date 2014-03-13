
SQL 2003 draft standard grammar review with examples and prototype
code.

We will go through the grammar for SQL:2003, try to understand and
provide concrete syntax examples and prototype parsing code, skip any
parts which aren't.

The starting point is this document
<http://savage.net.au/SQL/sql-2003-2.bnf.html>, combined with the
actual draft standard pdf files.

The section numbering here comes from the standard.

> import Text.Parsec.String
> import Text.Parsec.String.Char hiding (space)
> import Text.Parsec.String.Combinator
> import Control.Monad
> import Control.Applicative

= 5 Lexical elements

== 5.1 SQL terminal character

Here is the section:

```

Define the terminal symbols of the SQL language and the elements of strings.

<SQL terminal character>    ::=   <SQL language character>

<SQL language character>    ::=   <simple Latin letter> | <digit> | <SQL special character>

<simple Latin letter>    ::=   <simple Latin upper case letter> | <simple Latin lower case letter>

<simple Latin upper case letter>    ::= 
         A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

<simple Latin lower case letter>    ::= 
         a | b | c | d | e | f | g | h | i | j | k | l | m | n | o | p | q | r | s | t | u | v | w | x | y | z

<digit>    ::=   0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

<SQL special character>    ::= 
         <space>
     |     <double quote>
     |     <percent>
     |     <ampersand>
     |     <quote>
     |     <left paren>
     |     <right paren>
     |     <asterisk>
     |     <plus sign>
     |     <comma>
     |     <minus sign>
     |     <period>
     |     <solidus>
     |     <colon>
     |     <semicolon>
     |     <less than operator>
     |     <equals operator>
     |     <greater than operator>
     |     <question mark>
     |     <left bracket>
     |     <right bracket>
     |     <circumflex>
     |     <underscore>
     |     <vertical bar>
     |     <left brace>
     |     <right brace>

<space>    ::=   !! See the Syntax Rules.

<double quote>    ::=   "

<percent>    ::=   %

<ampersand>    ::=   &

<quote>    ::=   '

<left paren>    ::=   (

<right paren>    ::=   )

<asterisk>    ::=   *

<plus sign>    ::=   +

<comma>    ::=   ,

<minus sign>    ::=   -

<period>    ::=   .

<solidus>    ::=   /

<colon>    ::=   :

<semicolon>    ::=   ;

<less than operator>    ::=   <

<equals operator>    ::=   =

<greater than operator>    ::=   >

<question mark>    ::=   ?

The trigraphs are new in SQL-2003.

<left bracket or trigraph>    ::=   <left bracket> | <left bracket trigraph>

<right bracket or trigraph>    ::=   <right bracket> | <right bracket trigraph>

<left bracket>    ::=   [

<left bracket trigraph>    ::=   ??(

<right bracket>    ::=   ]

<right bracket trigraph>    ::=   ??)

<circumflex>    ::=   ^

<underscore>    ::=   _

<vertical bar>    ::=  /* Nothing */ |

<left brace>    ::=   {

<right brace>    ::=   }
```

Let's create some simple parsers to match this. We won't follow every
rule exactly with a data type and/or separate parser function, but we
won't try to hard to create a nice set of parsers and data types right
now either, just something convenient to write step by step, then it
can be refactored afterwoods.

So, this section contains grammar rules for the allowed characters in
SQL.

TODO: are there are some characters missing which can appear in sql
source and aren't here??


> sqlLanguageCharacter :: Parser Char
> sqlLanguageCharacter = upper <|> lower <|> digit <|> special

> special :: Parser Char
> special = choice
>           [space
>           ,doubleQuote
>           ,percent
>           ,ampersand
>           ,quote
>           ,leftParen
>           ,rightParen
>           ,asterisk
>           ,plusSign
>           ,comma
>           ,minusSign
>           ,period
>           ,solidus
>           ,colon
>           ,semicolon
>           ,lessThanOperator
>           ,greaterThanOperator
>           ,questionMark
>           ,leftBracket
>           ,rightBracket
>           ,circumflex
>           ,underscore
>           ,verticalBar
>           ,leftBrace
>           ,rightBrace
>           ]

> space
>           ,doubleQuote
>           ,percent
>           ,ampersand
>           ,quote
>           ,leftParen
>           ,rightParen
>           ,asterisk
>           ,plusSign
>           ,comma
>           ,minusSign
>           ,period
>           ,solidus
>           ,colon
>           ,semicolon
>           ,lessThanOperator
>           ,equalsOperator
>           ,greaterThanOperator
>           ,questionMark
>           ,leftBracket
>           ,rightBracket
>           ,circumflex
>           ,underscore
>           ,verticalBar
>           ,leftBrace
>           ,rightBrace :: Parser Char

space: Every character set shall contain a <space> character that is
equivalent to U+0020.

what are the consequences of this? where are all the non latin
letters? are there any other characters which are allowed, especially
operator characters,

> space = char ' '
> doubleQuote = char '"'
> percent = char '%'
> ampersand = char '&'
> quote = char '\''
> leftParen = char '('
> rightParen = char ')'
> asterisk = char '*'
> plusSign = char '+'
> comma = char ','
> minusSign = char '-'
> period = char '.'
> solidus = char '/'
> colon = char ':'
> semicolon = char ';'
> lessThanOperator = char '<'
> equalsOperator = char '='
> greaterThanOperator = char '>'
> questionMark = char '?'
> leftBracket = char '['
> rightBracket = char ']'
> circumflex = char '^'
> underscore = char '_'
> verticalBar = char '|'
> leftBrace = char '{'
> rightBrace = char '}'

I learnt a few new words there. I don't think we need any examples for
this section.

= 5.2 <token> and <separator>

Here is the complete text from the grammar:

```
Specifying lexical units (tokens and separators) that participate in SQL language.

<token>    ::=   <nondelimiter token> | <delimiter token>

<nondelimiter token>    ::= 
         <regular identifier>
     |     <key word>
     |     <unsigned numeric literal>
     |     <national character string literal>
     |     <bit string literal>
     |     <hex string literal>
     |     <large object length token>
     |     <multiplier>

<regular identifier>    ::=   <identifier body>

<identifier body>    ::=   <identifier start> [ <identifier part> ... ]

<identifier part>    ::=   <identifier start> | <identifier extend>

Previous standard said: 

<identifier start>    ::=   <initial alphabetic character> | <ideographic character>

<identifier start>    ::=   !! See the Syntax Rules.

<identifier extend>    ::=   !! See the Syntax Rules.

<large object length token>    ::=   <digit> ... <multiplier>

<multiplier>    ::=   K | M | G

<delimited identifier>    ::=   <double quote> <delimited identifier body> <double quote>

<delimited identifier body>    ::=   <delimited identifier part> ...

<delimited identifier part>    ::=   <nondoublequote character> | <doublequote symbol>

The productions for <Unicode delimited identifier> and so on are new in SQL-2003.

<Unicode delimited identifier>    ::= 
         U <ampersand> <double quote> <Unicode delimiter body> <double quote>
         <Unicode escape specifier>

<Unicode escape specifier>    ::=   [ UESCAPE <quote> <Unicode escape character> <quote> ]

<Unicode delimiter body>    ::=   <Unicode identifier part> ...

<Unicode identifier part>    ::=   <delimited identifier part> | <Unicode escape value>

<Unicode escape value>    ::= 
         <Unicode 4 digit escape value>
     |     <Unicode 6 digit escape value>
     |     <Unicode character escape value>

Syntax rule 20: <Unicode 4 digit escape value>'<Unicode escape character>+xyzw' is equivalent to the Unicode code point specified by U+xyzw.

<Unicode 4 digit escape value>    ::=   <Unicode escape character> <hexit> <hexit> <hexit> <hexit>

Syntax rule 21: <Unicode 6 digit escape value>'<Unicode escape character>+xyzwrs' is equivalent to the Unicode code point specified by U+xyzwrs.

NOTE 64: The 6-hexit notation is derived by taking the UCS-4 notation defined by ISO/IEC 10646-1 and removing the leading two hexits, whose values are always 0 (zero).

<Unicode 6 digit escape value>    ::= 
         <Unicode escape character> <plus sign> <hexit> <hexit> <hexit> <hexit> <hexit> <hexit>

Syntax rule 22: <Unicode character escape value> is equivalent to a single instance of <Unicode escape character>.

<Unicode character escape value>    ::=   <Unicode escape character> <Unicode escape character>

Syntax rule 15: <Unicode escape character> shall be a single character from the source language character set other than a <hexit>, <plus sign>, or <white space>.

Syntax rule 16: If the source language character set contains <reverse solidus>, then let DEC be <reverse solidus>; otherwise, let DEC be an implementation-defined character from the source language character set that is not a <hexit>, <plus sign>, <double quote>, or <white space>.

Syntax rule 17: If a <Unicode escape specifier> does not contain <Unicode escape character>, then "UESCAPE <quote>DEC<quote>" is implicit.

Syntax rule 18: In a <Unicode escape value> there shall be no <separator> between the <Unicode escape character> and the first <hexit>, nor between any of the <hexit>s.

<Unicode escape character>    ::=   !! See the Syntax Rules (15-18 above).

Syntax rule 6: A <nondoublequote character> is any character of the source language character set other than a <double quote>.

<nondoublequote character>    ::=   !! See the Syntax Rules.

The rule for <doublequote symbol> in the standard uses two adjacent literal double quotes rather than referencing <double quote>; the reasons are not clear. It is annotated '!! two consecutive double quote characters'.

<doublequote symbol>    ::=   <double quote> <double quote>

<delimiter token>    ::= 
         <character string literal>
     |     <date string>
     |     <time string>
     |     <timestamp string>
     |     <interval string>
     |     <delimited identifier>
     |     <Unicode delimited identifier>
     |     <SQL special character>
     |     <not equals operator>
     |     <greater than or equals operator>
     |     <less than or equals operator>
     |     <concatenation operator>
     |     <right arrow>
     |     <left bracket trigraph>
     |     <right bracket trigraph>
     |     <double colon>
     |     <double period>

The rules for <not equals operator> etc in the standard uses two adjacent literal characters rather than referencing <less than> and <greater than>; the reasons are not clear. Note that two characters must be adjacent with no intervening space, not a pair of characters separated by arbitrary white space.

<not equals operator>    ::=   <less than operator> <greater than operator>

<greater than or equals operator>    ::=   <greater than operator> <equals operator>

<less than or equals operator>    ::=   <less than operator> <equals operator>

<concatenation operator>    ::=   <vertical bar> <vertical bar>

<right arrow>    ::=   <minus sign> <greater than operator>

<double colon>    ::=   <colon> <colon>

<double period>    ::=   <period> <period>

<separator>    ::=   { <comment> | <white space> }...

<comment>    ::=   <simple comment> | <bracketed comment>

<simple comment>    ::=   <simple comment introducer> [ <comment character> ... ] <newline>

<simple comment introducer>    ::=   <minus sign> <minus sign> [ <minus sign> ... ]

The <bracketed comment> rule included '!! See the Syntax Rules'. This probably says something about the <slash> <asterisk> and <asterisk> <slash> needing to be adjacent characters rather than adjacent tokens.

<bracketed comment>    ::= 
         <bracketed comment introducer> <bracketed comment contents> <bracketed comment terminator>

<bracketed comment introducer>    ::=   <slash> <asterisk>

<bracketed comment terminator>    ::=   <asterisk> <slash>

<bracketed comment contents>    ::=   [ { <comment character> | <separator> }... ]

<comment character>    ::=   <nonquote character> | <quote>

<newline>    ::=   !! See the Syntax Rules.

There was a surprising amount of movement of keywords between the reserved and non-reserved word classes between SQL-99 and SQL-2003-2 FCD and again between SQL 2003-2 FCD and SQL 2003-2 IS. There is also room to think that much of the host language support moved out of Part 2 (SQL/Foundation).

<key word>    ::=   <reserved word> | <non-reserved word>
```

Let's start going through it step by step:

```
Specifying lexical units (tokens and separators) that participate in SQL language.

<token>    ::=   <nondelimiter token> | <delimiter token>

```

so like the sql terminal character and sql language character rules
above, the token rule is defining the token set used, and the token
rule is not really part of the grammar we need for parsing. Let's
implement it anyway.

> data Token = NonDelimiterToken NonDelimiterTokenType String
>            | DelimiterToken DelimiterTokenType String

> data NonDelimiterTokenType = RegularIdentifier
>                            | Keyword
>                            | UnsignedNumericLiteral
>                            | NationalCharacterStringLiteral
>                            | BitStringLiteral
>                            | HexStringLiteral
>                            | LargeObjectLengthToken
>                            | Multiplier

> token :: Parser Token
> token = nonDelimiterToken <|> delimiterToken

```

<nondelimiter token>    ::= 
         <regular identifier>
     |     <key word>
     |     <unsigned numeric literal>
     |     <national character string literal>
     |     <bit string literal>
     |     <hex string literal>
     |     <large object length token>
     |     <multiplier>

<regular identifier>    ::=   <identifier body>

<identifier body>    ::=   <identifier start> [ <identifier part> ... ]

<identifier part>    ::=   <identifier start> | <identifier extend>

Previous standard said: 

<identifier start>    ::=   <initial alphabetic character> | <ideographic character>

<identifier start>    ::=   !! See the Syntax Rules.

<identifier extend>    ::=   !! See the Syntax Rules.

<large object length token>    ::=   <digit> ... <multiplier>

<multiplier>    ::=   K | M | G

<delimited identifier>    ::=   <double quote> <delimited identifier body> <double quote>

<delimited identifier body>    ::=   <delimited identifier part> ...

<delimited identifier part>    ::=   <nondoublequote character> | <doublequote symbol>

The productions for <Unicode delimited identifier> and so on are new in SQL-2003.

<Unicode delimited identifier>    ::= 
         U <ampersand> <double quote> <Unicode delimiter body> <double quote>
         <Unicode escape specifier>

<Unicode escape specifier>    ::=   [ UESCAPE <quote> <Unicode escape character> <quote> ]

<Unicode delimiter body>    ::=   <Unicode identifier part> ...

<Unicode identifier part>    ::=   <delimited identifier part> | <Unicode escape value>

<Unicode escape value>    ::= 
         <Unicode 4 digit escape value>
     |     <Unicode 6 digit escape value>
     |     <Unicode character escape value>

Syntax rule 20: <Unicode 4 digit escape value>'<Unicode escape character>+xyzw' is equivalent to the Unicode code point specified by U+xyzw.

<Unicode 4 digit escape value>    ::=   <Unicode escape character> <hexit> <hexit> <hexit> <hexit>

Syntax rule 21: <Unicode 6 digit escape value>'<Unicode escape character>+xyzwrs' is equivalent to the Unicode code point specified by U+xyzwrs.

NOTE 64: The 6-hexit notation is derived by taking the UCS-4 notation defined by ISO/IEC 10646-1 and removing the leading two hexits, whose values are always 0 (zero).

<Unicode 6 digit escape value>    ::= 
         <Unicode escape character> <plus sign> <hexit> <hexit> <hexit> <hexit> <hexit> <hexit>

Syntax rule 22: <Unicode character escape value> is equivalent to a single instance of <Unicode escape character>.

<Unicode character escape value>    ::=   <Unicode escape character> <Unicode escape character>

Syntax rule 15: <Unicode escape character> shall be a single character from the source language character set other than a <hexit>, <plus sign>, or <white space>.

Syntax rule 16: If the source language character set contains <reverse solidus>, then let DEC be <reverse solidus>; otherwise, let DEC be an implementation-defined character from the source language character set that is not a <hexit>, <plus sign>, <double quote>, or <white space>.

Syntax rule 17: If a <Unicode escape specifier> does not contain <Unicode escape character>, then "UESCAPE <quote>DEC<quote>" is implicit.

Syntax rule 18: In a <Unicode escape value> there shall be no <separator> between the <Unicode escape character> and the first <hexit>, nor between any of the <hexit>s.

<Unicode escape character>    ::=   !! See the Syntax Rules (15-18 above).

Syntax rule 6: A <nondoublequote character> is any character of the source language character set other than a <double quote>.

<nondoublequote character>    ::=   !! See the Syntax Rules.

The rule for <doublequote symbol> in the standard uses two adjacent literal double quotes rather than referencing <double quote>; the reasons are not clear. It is annotated '!! two consecutive double quote characters'.

<doublequote symbol>    ::=   <double quote> <double quote>

<delimiter token>    ::= 
         <character string literal>
     |     <date string>
     |     <time string>
     |     <timestamp string>
     |     <interval string>
     |     <delimited identifier>
     |     <Unicode delimited identifier>
     |     <SQL special character>
     |     <not equals operator>
     |     <greater than or equals operator>
     |     <less than or equals operator>
     |     <concatenation operator>
     |     <right arrow>
     |     <left bracket trigraph>
     |     <right bracket trigraph>
     |     <double colon>
     |     <double period>

The rules for <not equals operator> etc in the standard uses two adjacent literal characters rather than referencing <less than> and <greater than>; the reasons are not clear. Note that two characters must be adjacent with no intervening space, not a pair of characters separated by arbitrary white space.

<not equals operator>    ::=   <less than operator> <greater than operator>

<greater than or equals operator>    ::=   <greater than operator> <equals operator>

<less than or equals operator>    ::=   <less than operator> <equals operator>

<concatenation operator>    ::=   <vertical bar> <vertical bar>

<right arrow>    ::=   <minus sign> <greater than operator>

<double colon>    ::=   <colon> <colon>

<double period>    ::=   <period> <period>

<separator>    ::=   { <comment> | <white space> }...

<comment>    ::=   <simple comment> | <bracketed comment>

<simple comment>    ::=   <simple comment introducer> [ <comment character> ... ] <newline>

<simple comment introducer>    ::=   <minus sign> <minus sign> [ <minus sign> ... ]

The <bracketed comment> rule included '!! See the Syntax Rules'. This probably says something about the <slash> <asterisk> and <asterisk> <slash> needing to be adjacent characters rather than adjacent tokens.

<bracketed comment>    ::= 
         <bracketed comment introducer> <bracketed comment contents> <bracketed comment terminator>

<bracketed comment introducer>    ::=   <slash> <asterisk>

<bracketed comment terminator>    ::=   <asterisk> <slash>

<bracketed comment contents>    ::=   [ { <comment character> | <separator> }... ]

<comment character>    ::=   <nonquote character> | <quote>

<newline>    ::=   !! See the Syntax Rules.

There was a surprising amount of movement of keywords between the reserved and non-reserved word classes between SQL-99 and SQL-2003-2 FCD and again between SQL 2003-2 FCD and SQL 2003-2 IS. There is also room to think that much of the host language support moved out of Part 2 (SQL/Foundation).

<key word>    ::=   <reserved word> | <non-reserved word>

<non-reserved word>    ::= 
         A 
     |     ABS 
     |     ABSOLUTE 
     |     ACTION 
     |     ADA 
     |     ADMIN 
     |     AFTER 
     |     ALWAYS 
     |     ASC 
     |     ASSERTION 
     |     ASSIGNMENT 
     |     ATTRIBUTE 
     |     ATTRIBUTES 
     |     AVG 
     |     BEFORE 
     |     BERNOULLI 
     |     BREADTH 
     |     C 
     |     CARDINALITY 
     |     CASCADE 
     |     CATALOG 
     |     CATALOG_NAME 
     |     CEIL 
     |     CEILING 
     |     CHAIN 
     |     CHARACTERISTICS 
     |     CHARACTERS 
     |     CHARACTER_LENGTH 
     |     CHARACTER_SET_CATALOG 
     |     CHARACTER_SET_NAME 
     |     CHARACTER_SET_SCHEMA 
     |     CHAR_LENGTH 
     |     CHECKED 
     |     CLASS_ORIGIN 
     |     COALESCE 
     |     COBOL 
     |     CODE_UNITS 
     |     COLLATION 
     |     COLLATION_CATALOG 
     |     COLLATION_NAME 
     |     COLLATION_SCHEMA 
     |     COLLECT 
     |     COLUMN_NAME 
     |     COMMAND_FUNCTION 
     |     COMMAND_FUNCTION_CODE 
     |     COMMITTED 
     |     CONDITION 
     |     CONDITION_NUMBER 
     |     CONNECTION_NAME 
     |     CONSTRAINTS 
     |     CONSTRAINT_CATALOG 
     |     CONSTRAINT_NAME 
     |     CONSTRAINT_SCHEMA 
     |     CONSTRUCTORS 
     |     CONTAINS 
     |     CONVERT 
     |     CORR 
     |     COUNT 
     |     COVAR_POP 
     |     COVAR_SAMP 
     |     CUME_DIST 
     |     CURRENT_COLLATION 
     |     CURSOR_NAME 
     |     DATA 
     |     DATETIME_INTERVAL_CODE 
     |     DATETIME_INTERVAL_PRECISION 
     |     DEFAULTS 
     |     DEFERRABLE 
     |     DEFERRED 
     |     DEFINED 
     |     DEFINER 
     |     DEGREE 
     |     DENSE_RANK 
     |     DEPTH 
     |     DERIVED 
     |     DESC 
     |     DESCRIPTOR 
     |     DIAGNOSTICS 
     |     DISPATCH 
     |     DOMAIN 
     |     DYNAMIC_FUNCTION 
     |     DYNAMIC_FUNCTION_CODE 
     |     EQUALS 
     |     EVERY 
     |     EXCEPTION 
     |     EXCLUDE 
     |     EXCLUDING 
     |     EXP 
     |     EXTRACT 
     |     FINAL 
     |     FIRST 
     |     FLOOR 
     |     FOLLOWING 
     |     FORTRAN 
     |     FOUND 
     |     FUSION 
     |     G 
     |     GENERAL 
     |     GO 
     |     GOTO 
     |     GRANTED 
     |     HIERARCHY 
     |     IMPLEMENTATION 
     |     INCLUDING 
     |     INCREMENT 
     |     INITIALLY 
     |     INSTANCE 
     |     INSTANTIABLE 
     |     INTERSECTION 
     |     INVOKER 
     |     ISOLATION 
     |     K 
     |     KEY 
     |     KEY_MEMBER 
     |     KEY_TYPE 
     |     LAST 
     |     LENGTH 
     |     LEVEL 
     |     LN 
     |     LOCATOR 
     |     LOWER 
     |     M 
     |     MAP 
     |     MATCHED 
     |     MAX 
     |     MAXVALUE 
     |     MESSAGE_LENGTH 
     |     MESSAGE_OCTET_LENGTH 
     |     MESSAGE_TEXT 
     |     MIN 
     |     MINVALUE 
     |     MOD 
     |     MORE 
     |     MUMPS 
     |     NAME 
     |     NAMES 
     |     NESTING 
     |     NEXT 
     |     NORMALIZE 
     |     NORMALIZED 
     |     NULLABLE 
     |     NULLIF 
     |     NULLS 
     |     NUMBER 
     |     OBJECT 
     |     OCTETS 
     |     OCTET_LENGTH 
     |     OPTION 
     |     OPTIONS 
     |     ORDERING 
     |     ORDINALITY 
     |     OTHERS 
     |     OVERLAY 
     |     OVERRIDING 
     |     PAD 
     |     PARAMETER_MODE 
     |     PARAMETER_NAME 
     |     PARAMETER_ORDINAL_POSITION 
     |     PARAMETER_SPECIFIC_CATALOG 
     |     PARAMETER_SPECIFIC_NAME 
     |     PARAMETER_SPECIFIC_SCHEMA 
     |     PARTIAL 
     |     PASCAL 
     |     PATH 
     |     PERCENTILE_CONT 
     |     PERCENTILE_DISC 
     |     PERCENT_RANK 
     |     PLACING 
     |     PLI 
     |     POSITION 
     |     POWER 
     |     PRECEDING 
     |     PRESERVE 
     |     PRIOR 
     |     PRIVILEGES 
     |     PUBLIC 
     |     RANK 
     |     READ 
     |     RELATIVE 
     |     REPEATABLE 
     |     RESTART 
     |     RETURNED_CARDINALITY 
     |     RETURNED_LENGTH 
     |     RETURNED_OCTET_LENGTH 
     |     RETURNED_SQLSTATE 
     |     ROLE 
     |     ROUTINE 
     |     ROUTINE_CATALOG 
     |     ROUTINE_NAME 
     |     ROUTINE_SCHEMA 
     |     ROW_COUNT 
     |     ROW_NUMBER 
     |     SCALE 
     |     SCHEMA 
     |     SCHEMA_NAME 
     |     SCOPE_CATALOG 
     |     SCOPE_NAME 
     |     SCOPE_SCHEMA 
     |     SECTION 
     |     SECURITY 
     |     SELF 
     |     SEQUENCE 
     |     SERIALIZABLE 
     |     SERVER_NAME 
     |     SESSION 
     |     SETS 
     |     SIMPLE 
     |     SIZE 
     |     SOURCE 
     |     SPACE 
     |     SPECIFIC_NAME 
     |     SQRT 
     |     STATE 
     |     STATEMENT 
     |     STDDEV_POP 
     |     STDDEV_SAMP 
     |     STRUCTURE 
     |     STYLE 
     |     SUBCLASS_ORIGIN 
     |     SUBSTRING 
     |     SUM 
     |     TABLESAMPLE 
     |     TABLE_NAME 
     |     TEMPORARY 
     |     TIES 
     |     TOP_LEVEL_COUNT 
     |     TRANSACTION 
     |     TRANSACTIONS_COMMITTED 
     |     TRANSACTIONS_ROLLED_BACK 
     |     TRANSACTION_ACTIVE 
     |     TRANSFORM 
     |     TRANSFORMS 
     |     TRANSLATE 
     |     TRIGGER_CATALOG 
     |     TRIGGER_NAME 
     |     TRIGGER_SCHEMA 
     |     TRIM 
     |     TYPE 
     |     UNBOUNDED 
     |     UNCOMMITTED 
     |     UNDER 
     |     UNNAMED 
     |     USAGE 
     |     USER_DEFINED_TYPE_CATALOG 
     |     USER_DEFINED_TYPE_CODE 
     |     USER_DEFINED_TYPE_NAME 
     |     USER_DEFINED_TYPE_SCHEMA 
     |     VIEW 
     |     WORK 
     |     WRITE 
     |     ZONE

<reserved word>    ::= 
         ADD 
     |     ALL 
     |     ALLOCATE 
     |     ALTER 
     |     AND 
     |     ANY 
     |     ARE 
     |     ARRAY 
     |     AS 
     |     ASENSITIVE 
     |     ASYMMETRIC 
     |     AT 
     |     ATOMIC 
     |     AUTHORIZATION 
     |     BEGIN 
     |     BETWEEN 
     |     BIGINT 
     |     BINARY 
     |     BLOB 
     |     BOOLEAN 
     |     BOTH 
     |     BY 
     |     CALL 
     |     CALLED 
     |     CASCADED 
     |     CASE 
     |     CAST 
     |     CHAR 
     |     CHARACTER 
     |     CHECK 
     |     CLOB 
     |     CLOSE 
     |     COLLATE 
     |     COLUMN 
     |     COMMIT 
     |     CONNECT 
     |     CONSTRAINT 
     |     CONTINUE 
     |     CORRESPONDING 
     |     CREATE 
     |     CROSS 
     |     CUBE 
     |     CURRENT 
     |     CURRENT_DATE 
     |     CURRENT_DEFAULT_TRANSFORM_GROUP 
     |     CURRENT_PATH 
     |     CURRENT_ROLE 
     |     CURRENT_TIME 
     |     CURRENT_TIMESTAMP 
     |     CURRENT_TRANSFORM_GROUP_FOR_TYPE 
     |     CURRENT_USER 
     |     CURSOR 
     |     CYCLE 
     |     DATE 
     |     DAY 
     |     DEALLOCATE 
     |     DEC 
     |     DECIMAL 
     |     DECLARE 
     |     DEFAULT 
     |     DELETE 
     |     DEREF 
     |     DESCRIBE 
     |     DETERMINISTIC 
     |     DISCONNECT 
     |     DISTINCT 
     |     DOUBLE 
     |     DROP 
     |     DYNAMIC 
     |     EACH 
     |     ELEMENT 
     |     ELSE 
     |     END 
     |     END-EXEC 
     |     ESCAPE 
     |     EXCEPT 
     |     EXEC 
     |     EXECUTE 
     |     EXISTS 
     |     EXTERNAL 
     |     FALSE 
     |     FETCH 
     |     FILTER 
     |     FLOAT 
     |     FOR 
     |     FOREIGN 
     |     FREE 
     |     FROM 
     |     FULL 
     |     FUNCTION 
     |     GET 
     |     GLOBAL 
     |     GRANT 
     |     GROUP 
     |     GROUPING 
     |     HAVING 
     |     HOLD 
     |     HOUR 
     |     IDENTITY 
     |     IMMEDIATE 
     |     IN 
     |     INDICATOR 
     |     INNER 
     |     INOUT 
     |     INPUT 
     |     INSENSITIVE 
     |     INSERT 
     |     INT 
     |     INTEGER 
     |     INTERSECT 
     |     INTERVAL 
     |     INTO 
     |     IS 
     |     ISOLATION 
     |     JOIN 
     |     LANGUAGE 
     |     LARGE 
     |     LATERAL 
     |     LEADING 
     |     LEFT 
     |     LIKE 
     |     LOCAL 
     |     LOCALTIME 
     |     LOCALTIMESTAMP 
     |     MATCH 
     |     MEMBER 
     |     MERGE 
     |     METHOD 
     |     MINUTE 
     |     MODIFIES 
     |     MODULE 
     |     MONTH 
     |     MULTISET 
     |     NATIONAL 
     |     NATURAL 
     |     NCHAR 
     |     NCLOB 
     |     NEW 
     |     NO 
     |     NONE 
     |     NOT 
     |     NULL 
     |     NUMERIC 
     |     OF 
     |     OLD 
     |     ON 
     |     ONLY 
     |     OPEN 
     |     OR 
     |     ORDER 
     |     OUT 
     |     OUTER 
     |     OUTPUT 
     |     OVER 
     |     OVERLAPS 
     |     PARAMETER 
     |     PARTITION 
     |     PRECISION 
     |     PREPARE 
     |     PRIMARY 
     |     PROCEDURE 
     |     RANGE 
     |     READS 
     |     REAL 
     |     RECURSIVE 
     |     REF 
     |     REFERENCES 
     |     REFERENCING 
     |     REGR_AVGX 
     |     REGR_AVGY 
     |     REGR_COUNT 
     |     REGR_INTERCEPT 
     |     REGR_R2 
     |     REGR_SLOPE 
     |     REGR_SXX 
     |     REGR_SXY 
     |     REGR_SYY 
     |     RELEASE 
     |     RESULT 
     |     RETURN 
     |     RETURNS 
     |     REVOKE 
     |     RIGHT 
     |     ROLLBACK 
     |     ROLLUP 
     |     ROW 
     |     ROWS 
     |     SAVEPOINT 
     |     SCROLL 
     |     SEARCH 
     |     SECOND 
     |     SELECT 
     |     SENSITIVE 
     |     SESSION_USER 
     |     SET 
     |     SIMILAR 
     |     SMALLINT 
     |     SOME 
     |     SPECIFIC 
     |     SPECIFICTYPE 
     |     SQL 
     |     SQLEXCEPTION 
     |     SQLSTATE 
     |     SQLWARNING 
     |     START 
     |     STATIC 
     |     SUBMULTISET 
     |     SYMMETRIC 
     |     SYSTEM 
     |     SYSTEM_USER 
     |     TABLE 
     |     THEN 
     |     TIME 
     |     TIMESTAMP 
     |     TIMEZONE_HOUR 
     |     TIMEZONE_MINUTE 
     |     TO 
     |     TRAILING 
     |     TRANSLATION 
     |     TREAT 
     |     TRIGGER 
     |     TRUE 
     |     UESCAPE 
     |     UNION 
     |     UNIQUE 
     |     UNKNOWN 
     |     UNNEST 
     |     UPDATE 
     |     UPPER 
     |     USER 
     |     USING 
     |     VALUE 
     |     VALUES 
     |     VAR_POP 
     |     VAR_SAMP 
     |     VARCHAR 
     |     VARYING 
     |     WHEN 
     |     WHENEVER 
     |     WHERE 
     |     WIDTH_BUCKET 
     |     WINDOW 
     |     WITH 
     |     WITHIN 
     |     WITHOUT 
     |     YEAR
```





---------------------------------------

> lexeme :: Parser a -> Parser a
> lexeme p = p <* whitespace

> whitespace :: Parser ()
> whitespace = void $ oneOf " \n\t"



TODO: see if can use the grammar to generate a parser using happy

#!/bin/bash

set -e

# this file is used to render the html via asciidoc from the source
# files and the README.asciidoc

mkdir -p build

set +e
rm build/IntroToParsing.lhs
set -e

cat GettingStarted.lhs VerySimpleExpressions.lhs ApplicativeStyle.lhs CombinatorReview.lhs FunctionsAndTypesForParsing.lhs TextParsecExpr.lhs AnIssueWithTokenParsers.lhs TextParsecPerm.lhs TextParsecToken.lhs ValueExpressions.lhs QueryExpressions.lhs FromClause.lhs SimpleSQLQueryParser0.lhs PrettyPrinting0.lhs ErrorMessages.lhs > build/IntroToParsing.lhs

echo > build/intro_to_parsing.asciidoc
echo :toc: right >> build/intro_to_parsing.asciidoc
echo :sectnums: >> build/intro_to_parsing.asciidoc
echo :toclevels: 10 >> build/intro_to_parsing.asciidoc
echo :source-highlighter: pygments >> build/intro_to_parsing.asciidoc
echo >> build/intro_to_parsing.asciidoc

cat README.asciidoc | runhaskell render/FixReadme.lhs >> build/intro_to_parsing.asciidoc
cat build/IntroToParsing.lhs | runhaskell render/Render.lhs >> build/intro_to_parsing.asciidoc

cat build/intro_to_parsing.asciidoc | asciidoctor -d book - | runhaskell AddLinks.lhs > build/index.html

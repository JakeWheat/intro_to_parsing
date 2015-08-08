#!/bin/bash

set -e

#test code to render to html to see what it looks like

mkdir -p build

cp main.css build

for i in `find . -iname '*hs'`; do
    echo $i
    mkdir -p build/$(dirname $i)
    runhaskell render/Render.lhs $i > build/${i%.*}.asciidoc && \
        asciidoctor build/${i%.*}.asciidoc
done

echo README.asciidoc

(cd render && cabal sandbox init && cabal install split)

cat README.asciidoc | runhaskell -package-db=render/.cabal-sandbox/x86_64-linux-ghc-7.10.2-packages.conf.d/ render/FixReadme.lhs | asciidoctor -o build/index.html -

rm build/*.asciidoc

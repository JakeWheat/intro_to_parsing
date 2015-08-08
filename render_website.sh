#!/bin/bash

set -e

#test code to render to html to see what it looks like

mkdir -p build

cp main.css build

# need some mkdir -p

for i in `find . -iname '*hs'`; do
    echo $i
    mkdir -p build/$(dirname $i)
    runhaskell render/Render.lhs $i > build/${i%.*}.asciidoc && \
        asciidoctor build/${i%.*}.asciidoc
done

rm build/*.asciidoc

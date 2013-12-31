#!/bin/sh

#test code to render to html to see what it looks like

mkdir -p html/

cp main.css html/

for i in *.lhs; do
    pandoc --from=markdown+lhs --to=html $i -o html/$i.html -c main.css --toc;
done

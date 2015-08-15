
Little hack to add links to the navigation bars when generating the
html website.

> main :: IO ()
> main = interact addLinks


> addLinks :: String -> String
> addLinks [] = error "not found"
> addLinks ('<':'/':'u':'l':'>':'\n':'<':'/':'d':'i':'v':'>':xs) =
>     "</ul>" ++ linkSection ++ "\n</div>" ++ xs
> addLinks (x:xs) = x : addLinks xs

> linkSection :: String
> linkSection =
>   "<hr />\n\
>   \<ul class=\"sectlevel1\">\n\
>   \<li><a href=\"http://jakewheat.github.io/intro_to_parsing\" class=\"bare\">Homepage</a></li>\n\
>   \<li><a href=\"https://github.com/JakeWheat/intro_to_parsing\" class=\"bare\">Repository</a></li>\n\
>   \<li><a href=\"https://github.com/JakeWheat/intro_to_parsing/issues\" class=\"bare\">Bug tracker</a></li>\n\
>   \<li><a href=\"http://jakewheat.github.io/\" class=\"bare\">Parent project</a>\n\
>   \</li><li>jakewheatmail@gmail.com</li>\n\
>   \</ul>\n"


This code is to take the .lhs and add some formatting so it renders
nicely with asciidoctor.

TODO

concat docs then render as multiple pages
get the inter/intra doc links working

find a better fix for the * issue

> import System.Environment
> import Data.Char
> import System.IO
> import System.FilePath
> import Data.List

> main :: IO ()
> main = do
>     --[fn] <- getArgs
>     src <- getContents -- readFile fn
>     --let title = makeTitle $ takeBaseName fn
>     --putStrLn $ ":toc: right\n:toclevels: 8\n:sectnums:\n"
>     --putStrLn "= Introduction to parsing in Haskell with Parsec\n"
>     putStrLn . unlines . map fixHeadings
>       . addBlocks False . lines $ src
>   where
>     makeTitle (c:c1:cs) | isUpper c && isUpper c1 =
>       let (rest,cs') = span isUpper cs
>       in case (rest,cs') of
>             ([],_) -> ' ' : c : ' ' : c1 : makeTitle cs'
>             (_, d:_) | isDigit d -> ' ' : c : c1 :
>                                     (rest ++ " " ++  makeTitle cs')
>             _ -> ' ' : c : c1 : (init rest ++  " " ++ [last rest]
>                                    ++ makeTitle cs')
>     makeTitle (c:cs) | isUpper c = ' ' : toUpper c : makeTitle cs
>     makeTitle (c:cs) = c : makeTitle cs
>     makeTitle [] = []
>     fixHeadings ('=':xs) = '=':'=':xs
>     fixHeadings xs = xs
>     addBlocks False (l:ls)
>         | isLiterate l =
>             "[source,haskell]\n----" : stripLiterate l
>             : addBlocks True ls
>         | otherwise = l : addBlocks False ls

>     addBlocks True (l:ls)
>         | isLiterate l =
>             stripLiterate l : addBlocks True ls
>         | otherwise = "----" : l : addBlocks False ls
>     addBlocks _ [] = []
>     stripLiterate ('>':' ':l) = l
>     stripLiterate (">") = ""
>     stripLiterate l = l
>     isLiterate ('>':_) = True
>     isLiterate _ = False

escape asterisk: rules worked out by experiment. If there is an
asterisk on a non code block line, and there are more asterisks on the
line, then put a backslash before the first asterisk only to stop it
from being interpreted as bold formatting. Not sure why only the first
one should have a backslash, or why if you put a backslash in front of
subsequence asterisks, then the backslash is rendered ...

Doesn't work, need to parse paragraphs not lines, todo, and need to
make sure it doesn't replace inside blocks

>     {-escapeAsterisk l
>         | length (elemIndices '*' l) > 1 =
>               let (a,b) = break (=='*') l
>               in a ++ ('\\':b)
>         | otherwise = l-}

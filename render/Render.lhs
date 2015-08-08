
This code is to take the .lhs and add some formatting so it renders
nicely with asciidoctor.

TODO: remove the bird feet also?

fix headings
add toc stuff
fix the build website

syntax highlighing

asterisk issues: maybe just escape the first on on the line
if it is not source
and there are more * on that line

> import System.Environment
> import Data.Char
> import System.IO
> import System.FilePath
> import Data.List

> main :: IO ()
> main = do
>     [fn] <- getArgs
>     src <- readFile fn
>     let title = makeTitle $ takeBaseName fn
>     putStrLn $ ":toc: right\n:toclevels: 8\n= " ++ title ++ "\n"
>     putStrLn . unlines . map fixHeadings . addBlocks . lines $ src
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
>     addBlocks (l0:l1:ls) | not (isLiterate l0) && isLiterate l1 =
>       l0:"[source,haskell]\n----":addBlocks (l1:ls)
>     addBlocks (l0:l1:ls)| isLiterate l0 && not (isLiterate l1) =
>       l0:"----":addBlocks (l1:ls)
>     addBlocks (l:ls) = l : addBlocks ls
>     addBlocks [] = []
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

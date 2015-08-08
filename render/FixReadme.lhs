
Apply some small fixes to the README.asciidoc so that this file can be
written to work on the github repo, and then we fix the links to that
it also serves as a good index.html for the rendered website.


> import Data.List
> import Data.List.Split

> main :: IO ()
> main = do
>     putStrLn ":toc: right\n"
>     ls <- getContents
>     putStrLn $ unlines $ map fixLinks $ lines ls

> fixLinks x | "link:" `isPrefixOf` x =
>     replace ".lhs" ".html" x
> fixLinks x = x

> replace old new = intercalate new . splitOn old


Fix the README for the html genereation

> import Data.List

> main :: IO ()
> main = do
>     ls <- getContents
>     putStrLn $ unlines $ map fixLinks $ lines ls

> fixLinks x | "link:" `isPrefixOf` x = []
> fixLinks x = x

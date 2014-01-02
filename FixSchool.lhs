
read an lhs file, and wrap the > sections in ```haskell...```

todo: fix > at end of file not getting ``` closing delimiter

> import Debug.Trace
>
> main :: IO ()
> main = do
>     interact (unlines . addTicks . lines)
>   where
>     addTicks (l0:l1:ls) | not (isLiterate l0) && isLiterate l1 =
>       l0:"```haskell":addTicks (l1:ls)
>     addTicks (l0:l1:ls)| isLiterate l0 && not (isLiterate l1) =
>       l0:"```":addTicks (l1:ls)
>     addTicks (l:ls) = l:addTicks ls
>     addTicks [] = []
>     isLiterate ('>':_) = True
>     isLiterate _ = False

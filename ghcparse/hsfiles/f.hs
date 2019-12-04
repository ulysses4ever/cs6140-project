module Foo where

--g = -1

f 0 = 1
--f (-1) = 1
f n = n * f (n - 1)

reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

z :: Int -> Int
z n = 
    case n of
        0 -> 0
        m -> m

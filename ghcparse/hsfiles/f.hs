module Foo where

g = bar
  where 
	bar = 5
	bar :: Int

f 0 = 1
--f (-1) = 1
f n | n > 0 = n * f (n - 1)
f x = x * f (x - 1) >>= 1

reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

z :: Int -> Int
z n = 
    case n of
        0 -> 0
        m -> m

f 0 = 1
f n = n * f (n - 1)

reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

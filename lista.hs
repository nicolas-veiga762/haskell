--8

menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs

--9

removerElem _ [] = []
removerElem n (x:xs) = if n == x then removerElem n xs else x:removerElem n xs

--10

ordena [] = []
ordena xs = (menor xs):ordena (removerElem (menor xs) xs) 

--11

ins n [] = [n]
ins n (x:xs) | n == x = x:xs
             | n < x = [n] ++ (x:xs)
             | otherwise = x:ins n xs

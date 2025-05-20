--1
nprimeiros 0 xs = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs

--2
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

ultimo [] = []
ultimo (x:xs) = nprimeiros 1 (inverso(x:xs))

--3
auxiliar 0 _ = []
auxiliar n x = x : auxiliar (n-1) x

replicar _ [] = []
replicar n (x:xs) = auxiliar n x ++ replicar n xs

--4
fatiar _ _ [] = [] 
fatiar i f (x:xs)
    | i > 0   = fatiar (i-1) (f-1) xs  
    | f >= 0     = x : fatiar i (f-1) xs  
    | otherwise    = []  
    
--5
menores [] = []
menores ((x,y):xs) = if x < y then (x,y):menores xs else menores xs

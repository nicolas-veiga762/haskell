--1'descritiva'

--2
pares [] = []
pares (x:xs) = if mod x 2 == 1 then x:pares xs else pares xs

--3
remover _ [] = []
remover a (x:xs) = if a == x then remover a xs else x:remover a xs 

--4
todos [] = True
todos (x:xs) = if x == False then False else todos xs

--5
segundos [] = []
segundos ((x,y):xs) = [y]:segundos xs

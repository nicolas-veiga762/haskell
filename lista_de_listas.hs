--1
pertence e [] = False
pertence e (x:xs) = if e == x then True else pertence e xs

--2
intercessao ::(Eq a)=> [a] -> [a] -> [a]
intercessao [] ys = []
intercessao (x:xs) ys = if pertence x ys then x:intercessao xs ys else intercessao xs ys

--3
inverso :: [a] -> [a]
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

--4
nPrimeiros n [] = []
nPrimeiros 0 (x:xs) = []
nPrimeiros n (x:xs) = x:nPrimeiros (n-1) xs
nUltimos n (x:xs) = inverso(nPrimeiros n(inverso xs))

--5
soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = (x+y) ++ soma2 xs ys

--6
pot2 :: Int -> [Int]
pot2 n = [2 ^x | x <- [1..n]]

--7
intercalacao _ [] = [] ++ []
intercalacao [] _ = [] ++ []
intercalacao (x:xs) (y:ys) = if x < y then x:intercalacao xs (y:ys) else y:intercalacao (x:xs) ys

--8
menor [] = []
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
             
--12
enesimo 1 (x:xs) = x
enesimo n (x:xs) = enesimo (n-1) xs

--13
repetir 0 _ = []
repetir n e = e: repetir (n-1) e

--14
numString 0 = []
numString n = numString (div n 10) ++ [int2Char (rem n 10)]
int2Char :: Int -> Char
int2Char d = toEnum (d+48)

--15
stringNum :: String -> Int
stringNum "" = 0
stringNum (x:xs) = (fromEnum x - fromEnum '0') * 10 ^ length xs + stringNum xs

--16 
bin2int :: String -> Int
bin2int "" = 0
bin2int (x:xs) = (fromEnum x - fromEnum '0') * 2 ^ length xs + bin2int xs

--17
int2bin :: Int -> String
int2bin 0 = "0"
int2bin n = reverse (int2bin' n)
  where
    int2bin' 0 = []
    int2bin' n = (toEnum (fromEnum '0' + n `mod` 2) :: Char) : int2bin' (n `div` 2)


--18
minusculas :: String -> String
minusculas [] = []
minusculas (x:xs)
  | x >= 'A' && x <= 'Z' = (toEnum (fromEnum x + 32) :: Char) : minusculas xs
  | otherwise = x : minusculas xs


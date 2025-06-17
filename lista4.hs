-- funções para aprender: foldr, filter, zip, repeat, words, lines, all, map

-- b
numLines xs = numLines' 1 xs
numLines' _ [] = []
numLines' n (x:xs) = (n, x):numLines' (n+1) xs

-- c
allNumWords [] = []
allNumWords ((n, x):xs) = zip (repeat n) (words x) ++ allNumWords xs

--d
sortLs [] = []
sortLs = 

-- funções para aprender: foldr, filter, zip, repeat, words, lines, all, map, compare

import Data.List (sortBy, groupBy)
import Data.Function (on)

type Doc = String
type Line = String
type Word' = String

--a função do proprio haskell

-- b
numLines xs = numLines' 1 xs
numLines' _ [] = []
numLines' n (x:xs) = (n, x):numLines' (n+1) xs

-- c
allNumWords [] = []
allNumWords ((n, x):xs) = zip (repeat n) (words x) ++ allNumWords xs

--d
sortLs [] = []
sortLs x = sortBy (\(_, a) (_, b) -> compare a b) x

--e
almalgamate xs = map (\ws -> (map fst ws, snd (head ws))) (groupBy (\x y -> snd x == snd y) (sortLs xs))

--f
shorten = map (\(ns, w) -> (unique ns, w))
unique = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

--makeindex
makeindex txt = shorten . almalgamate . sortLs . allNumWords . numLines $ txt

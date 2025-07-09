import Data.Char (toUpper)

-- 1. Retorna o maior entre três números
maior3 a b c = if a>b && a>c then a else if b>a && b>c then b else c 

-- 2. Verifica se um número é par ou ímpar
parImpar n = if n `mod` 2 == 0 then "par" else "impar"

-- 3. Calcula o fatorial de um número
aux1 a = if a <= 1 then [1] else a : aux1 (a -1)
fatorial n = foldr (*) 1 (aux1 n)

-- 5. Verifica se uma string é um palíndromo
palindromo s = if s == reverse s then True else False

-- 6. Conta quantas vezes um elemento aparece na lista
aux2 n x = n == x
contaElem n xs = length (filter (aux2 n) xs)

-- 7. Dobra apenas os números pares da lista
aux3 x = x `mod` 2 == 0
aux4 x = x*2
dobrarPares [] = []
dobrarPares (x:xs) = map aux4 (filter (aux3) xs) 

-- 8. Verifica se todos os elementos da lista são positivos
todosPositivos [] = True
todosPositivos (x:xs) = if x < 0 then False else todosPositivos xs

-- 9. Soma todos os múltiplos de 3 da lista
aux5 x = x `mod` 3 == 0
somaMultiplosDe3 xs = foldr (+) 0 (filter (aux5) xs)

-- 10. Insere um elemento em uma lista ordenada mantendo a ordem
inserirOrdenado e [] = [e]  
inserirOrdenado e (x:xs) = if e <= x then e:x:xs else x : inserirOrdenado e xs

-- 11. Converte todas letras minúsculas em maiúsculas
maiusculas s = map toUpper s

-- 12. Conta o número de vogais em uma string
aux6 c = c `elem` "aeiouAEIOU"
contaVogais s = length (filter (aux6) s)

-- 13. Remove todos os espaços de uma string
removeEspacos :: String -> String
removeEspacos s = filter (/= ' ') s

-- 14. Inverte a ordem das palavras de uma frase
reversoPalavras s = unwords (reverse (words s))

-- 15. Extrai apenas os números de uma string
aux7 a = a `elem` "1234567890"
soNumeros s = filter (aux7) s

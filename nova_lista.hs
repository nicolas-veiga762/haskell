-- 1. Retorna o maior entre três números
maior3 a b c = if a>b && a>c then a else if b>a && b>c then b else c 

-- 2. Verifica se um número é par ou ímpar
parImpar n = if n `mod` 2 == 0 then "par" else "impar"

-- 3. Calcula o fatorial de um número
aux1 :: Integer -> [Integer]
aux1 a = if a <= 1 then [1] else a : aux1 (a -1)
fatorial n = foldr (*) 1 (aux1 n)

-- 5. Verifica se uma string é um palíndromo
palindromo s = if s == reverse s then True else False

-- 6. Conta quantas vezes um elemento aparece na lista
aux3 n x = n == x
contaElem n xs = length (filter (aux3 n) xs)

-- 7. Dobra apenas os números pares da lista
dobrarPares xs = undefined

-- 8. Verifica se todos os elementos da lista são positivos
todosPositivos xs = undefined

-- 9. Soma todos os múltiplos de 3 da lista
somaMultiplosDe3 xs = undefined

-- 10. Insere um elemento em uma lista ordenada mantendo a ordem
inserirOrdenado e xs = undefined

-- 11. Converte todas letras minúsculas em maiúsculas
maiusculas s = undefined

-- 12. Conta o número de vogais em uma string
contaVogais s = undefined

-- 13. Remove todos os espaços de uma string
removeEspacos s = undefined

-- 14. Inverte a ordem das palavras de uma frase
reversoPalavras s = undefined

-- 15. Extrai apenas os números de uma string
soNumeros s = undefined

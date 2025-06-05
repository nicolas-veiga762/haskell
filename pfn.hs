--funções de ordem superior

filter' _ [] = []
filter' p (x:xs) = if p x then x:filter' p xs else filter' p xs

par x = rem x 2 == 0

-- coloque no terminal: filter' (\x -> rem x 2 == 0) [2, 10, 11 4]
-- a função lâmbida está declarando exatamente o que a função par, mas agora dentro da função filter'. Na biblioteca, a função filter recebe dois parâmetros e checa se a operação entre eles é verdadeira ou não (se for verdadeira ele cria uma lista com o valor verdadeiro, se for falso ele retorna pra recursão). Por isso quando se escreve "filter' (\x -> rem x 2 == 0) [2, 10, 11 4]" no terminal, vai retornar [2, 10, 4], pois na condição da função lâmbida, estes números retornaram True para o filter, então ele criou uma lista com eles.

-- podemos fazer a função filter' (\x -> not (par x)) [2, 10, 11, 4], que vai retornar uma lista com os ímpares

-- "." equivale a (\g\f\x.g(f x)). Usando com "not" e "par", ela fica (\g\f\x.g(f x)) not par, resultando em (\x not (par x)), que funciona no terminal como (\x -> not (par x)). Isso também pode ser escrito como "(.) not par" ou então "not.par"

g <.> f = \x -> g (f x)

somatorio [] = 0
somatorio (x:xs) = x + somatorio xs

produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

-- foldr = fold right, faz as operações começando pelo número mais a direita da lista
-- ex de meuFoldr no terminal: meuFoldr (&&) True [True, True, True]
meuFoldr f b [] = b
meuFoldr f b (x:xs) = f x (meuFoldr f b xs)

all' [] = True
all' (x:xs) = if x == False then False else all' xs

all'' xs = meuFoldr (&&) True xs



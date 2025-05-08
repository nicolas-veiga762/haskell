--Exercicio proposto em aula
zip' [] ys = []
zip' xs [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

--Outro exercicio proposto em sala
tabuada = tabAux1 1
tabAux1 9 = tabAux2 9 0
tabAux1 n =  tabAux2 n 1 ++ tabAux1 (n+1)
tabAux2 n1 9 = [(n1,9,n1*9)]
tabAux2 n1 n2 = (n1, n2, n1*n2): tabAux2 n1 (n2 + 1)

--[(x,y, x*y)| x<- [1..9],y<-[1..9]]




{- Crie um programa que recebe uma lista de inteiros e um 
   número inteiro positivo i e retorna o i-ésimo elemento da lista.
  -}

iesimo :: [Int] -> Int -> Int
iesimo (head:body) index = if index == 1 then head else iesimo (body) (index - 1)

main = do
  lista <- getLine
  index <- getLine
  print (iesimo (read lista) (read index))

{- Crie um programa que recebe duas listas e retorna o conjunto união das duas.
  -}

existe :: Int -> [Int] -> Bool
existe num [] = False
existe num (head:body) = if head == num then True else existe num body

sem_intercessao :: [Int] -> [Int] -> [Int]
sem_intercessao [] l1 = []
sem_intercessao l2 [] = l2
sem_intercessao (head:body) l1 = 
    if existe head l1 then 
        [] ++ sem_intercessao body l1
    else
        [head] ++ sem_intercessao body l1

uniao :: [Int] -> [Int] -> [Int]
uniao l1 l2 = l1 ++ sem_intercessao l2 l1

main = do
  lista1 <- getLine
  lista2 <- getLine
  print (uniao (read lista1) (read lista2))

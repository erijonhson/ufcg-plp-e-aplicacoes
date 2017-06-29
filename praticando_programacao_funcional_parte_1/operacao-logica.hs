{- Crie um programa que recebe três valores booleanos e uma string que 
   representa a operação lógica correpondente a ser aplicada (“AND” ou “OR”).
  -}

andOperation :: Bool -> Bool -> Bool -> Bool
andOperation a b c = if a && b && c then True else False

orOperation :: Bool -> Bool -> Bool -> Bool
orOperation a b c = if a || b || c then True else False

calcula :: Bool -> Bool -> Bool -> String -> Bool
calcula a b c operacao = if operacao == "AND" then andOperation a b c else orOperation a b c

main = do
  valor1 <- getLine
  valor2 <- getLine
  valor3 <- getLine
  operacao <- getLine
  print (calcula (read valor1) (read valor2) (read valor3) operacao)
  

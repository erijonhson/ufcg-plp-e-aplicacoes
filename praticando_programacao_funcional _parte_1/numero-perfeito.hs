{- Um número inteiro não negativo diz-se perfeito se é igual à soma dos seus 
   divisores próprios. Por exemplo, 6=2+3+1. Faça um programa que recebe 
   um inteiro e decide se este é ou não perfeito.
  -}

divisoresDeNumero :: Int -> [Int]
divisoresDeNumero num = 
    [ x | x <- [1 .. metadeNumero], ((mod num x) == 0)]
    where metadeNumero = floor(fromIntegral num / 2)

ehPerfeito :: Int -> Bool
ehPerfeito num = if ((sum (divisoresDeNumero num)) == num) then True else False

main = do
  valor1 <- getLine
  print (ehPerfeito (read valor1))
  

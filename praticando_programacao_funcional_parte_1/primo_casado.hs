{- Se n e n+2 são números primos, então dizem-se primos casados. 
   Escreva um programa usando funções que receba um inteiro e determine se este é: 
   "NAO PRIMO!", "PRIMO!" ou "PRIMO CASADO!".
 -}

-- mesmo algoritmo da questão anterior
divisoresDeNumero :: Int -> [Int]
divisoresDeNumero num = 
    [ x | x <- [1 .. metadeNumero], ((mod num x) == 0)]
    where metadeNumero = floor(fromIntegral num / 2)

ehPrimo :: Int -> Bool
ehPrimo n = if (divisoresDeNumero n) == [1] then True else False

ehCasado :: Int -> String
ehCasado n
    | ehPrimo(nSoma) && ehPrimo(n) = "PRIMO CASADO!"
    | ehPrimo(n) = "PRIMO!"
    | otherwise = "NAO PRIMO!"
    where nSoma = n + 2

main = do
    valor1 <- getLine
    let result = ehCasado (read valor1)
    print result
    

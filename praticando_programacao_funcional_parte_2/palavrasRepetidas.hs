{- Crie um programa que, a partir de uma lista de palavras, 
   retorna uma lista de tuplas do tipo (palavra, quantidade), 
   onde "palavra" é uma palavra da lista e "quantidade" é o seu número de repetições 
   dessa palavra na lista. Deve ser preservada a ordem das palavras na lista original.
  -}

quantidade :: String -> [String] -> Int
quantidade palavra [] = 0
quantidade palavra (head:body) =
    if head == palavra then 
        1 + quantidade palavra body
    else 
        0 + quantidade palavra body

removeElemento :: String -> [String] -> [String]
removeElemento palavra [] = []
removeElemento palavra lista = [p | p <- lista, p /= palavra]

retornaTuplas :: [String] -> [(String, Int)]
retornaTuplas [] = []
retornaTuplas (head:body) = do
    let quant = (quantidade head body) + 1
    let lista = removeElemento head body
    [(head, quant)] ++ retornaTuplas lista

main = do
  palavras <- getLine
  print (retornaTuplas (read palavras))

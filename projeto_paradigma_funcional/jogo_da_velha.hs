{- Tema 8: Jogo da Velha
 - 
 - O jogo consiste de um tabuleiro, sendo este uma matriz de três linha e três colunas.
 - A seguir, a especificação do jogo:
 - a. São 2 participantes.
 - b. Um começa o jogo.
 - c. Um joga após o outro, até o fim do jogo.
 - d. Em cada jogada é colocado um símbolo ("O" ou "X") em uma posição não ocupada do tabuleiro.
 - e. No início do jogo, todas as posições do tabuleiro estão livres/desocupadas.
 - f. O jogo termina quando se forma uma sequência de três símbolos do mesmo tipo em 
 -   linha horizontal, vertical ou diagonal. Quando isto acontece, o jogador que colocou 
 -   o símbolo ganha o jogo (e o outro perde)
 - g. O jogo também pode terminar quando alguém preenche o último espaço disponível 
 -   (neste caso quem ganha é a "velha").
 - 
 - A lógica do jogo é muito simples, de modo que não é difícil deduzir ou decorar todas 
 - as possibilidades para efetuar a melhor jogada - apesar de o número total de possibilidades ser 
 - muito grande, a maioria delas é simétrica, além de que as regras são simples.
 - 
 - Implemente as seguintes regras por ordem de prioridade:
 - 1. Ganhar: se você tem duas peças numa linha, ponha a terceira.
 - 2. Bloquear: se o oponente tiver duas peças em linha, ponha a terceira para bloqueá-lo.
 - 3. Triângulo: crie uma oportunidade em que você poderá ganhar de duas maneiras.
 - 4. Bloquear o Triângulo do oponente:
 - - Opção 1: crie 2 peças em linha para forçar o oponente a se defender, 
 -   contanto que não resulte nele criando um triângulo ou vencendo. 
 -   Por exemplo, se 'X' tem dois cantos opostos do tabuleiro e 'O' tem o centro, 'O' 
 -   não pode jogar num canto (jogar no canto nesse cenário criaria um triângulo em que 'X' vence).
 - - Opção 2: se existe uma configuração em que o oponente pode formar um triângulo, bloqueiem-no.
 - 5. Centro: jogue no centro.
-}

-- Possível ajuda: https://gist.github.com/marcoscastro/d6b0be7f6cf600fdb53b
-- Problemas com print fora do main: https://wiki.haskell.org/Introduction_to_Haskell_IO/Actions
-- See Plataform: http://haskell.tailorfontela.com.br/introduction
-- Sobre arrays: https://www.haskell.org/tutorial/arrays.html
-- reference: http://zvon.org/other/haskell/Outputglobal/index.html
-- exercícios sobre matrizes: http://www.glc.us.es/~jalonso/vestigium/i1m2012-ejercicios-sobre-matrices-en-haskell-2/

import System.IO
import System.IO.Error
import System.Process
import Data.Array

-- sinônimos
type Marcacao = String                        -- Sinônimo para marcação no tabuleiro
type Posicao = (Int, Int)                     -- Sinônimo para uma posição escolhida
type Tabuleiro = Array Posicao Marcacao       -- Sinônimo para Tabuleiro

-- constantes 
vazio :: Marcacao
vazio = "-"
primeiroJogador :: Marcacao
primeiroJogador = "X"
segundoJogador :: Marcacao
segundoJogador = "O"

-- função principal
main = do
    instrucoes
    -- tabuleiro, índices iniciam em (1,1) e terminam em (3,3)
    let tabuleiro = array ((1,1),(3,3)) [((x,y), vazio) | x <- [1,2,3], y <- [1,2,3]]
    resultado <- turnoJogador tabuleiro primeiroJogador
    putStrLn resultado

-- auxiliares de main

instrucoes = do
    system "cls" -- limpa a tela (windows somente) 
    putStrLn "Bem-vindos ao Jogo da Velha!\n"
    putStrLn "As regras sao bem simples:"
    putStrLn "O jogo consiste de um tabuleiro, sendo este uma matriz de tres linhas e tres colunas."
    putStrLn "Sao 2 participantes (O e X). Um comeca o jogo e um joga apos o outro, ate o fim do jogo."
    putStrLn "Em cada jogada e colocado um simbolo ('O' ou 'X') em uma posicao nao ocupada do tabuleiro."
    putStrLn "No inicio do jogo, todas as posicoes do tabuleiro estao livres/desocupadas."
    putStrLn "O jogo termina quando se forma uma sequencia de tres simbolos do mesmo tipo em linha horizontal, vertical ou diagonal."
    putStrLn "Quando isto acontece, o jogador que colocou o simbolo ganha o jogo (e o outro perde)"
    putStrLn "O jogo tambem pode terminar quando alguem preenche o ultimo espaco disponível (neste caso quem ganha e a 'velha').\n\n"

turnoJogador :: Tabuleiro -> Marcacao -> IO String 
turnoJogador tab jogador = do
    imprimeTabuleiro tab
    tabAtualizado <- jogadorJoga tab jogador
    let vencedor = verificaVitoria tabAtualizado jogador
    if vencedor then do
        imprimeTabuleiro tabAtualizado
        return("Parabens, jogador " ++ jogador ++ "! Voce venceu!")
    else do
        if deuVelha tabAtualizado then do
            imprimeTabuleiro tabAtualizado
            return ("Deu velha!")
        else
            turnoJogador tabAtualizado (oponente jogador)

-- fim auxiliares de main

-- auxiliares de turnoJogador

imprimeTabuleiro :: Tabuleiro -> IO ()
imprimeTabuleiro tab = do
    putStrLn "--- JOGO DA VELHA ---\n"
    putStr "    1   2   3"
    putStrLn ("\n1   " ++ tab ! (1,1) ++ "   " ++ tab ! (1,2) ++ "   " ++ tab ! (1,3)
           ++ "\n2   " ++ tab ! (2,1) ++ "   " ++ tab ! (2,2) ++ "   " ++ tab ! (2,3)
           ++ "\n3   " ++ tab ! (3,1) ++ "   " ++ tab ! (3,2) ++ "   " ++ tab ! (3,3))

jogadorJoga :: Tabuleiro -> Marcacao -> IO Tabuleiro
jogadorJoga tab jogador = do
    menu
    posicao <- processaEntrada jogador tab
    let tabAtualizado = tab // [(posicao,vazio), (posicao,jogador)] 
    return (tabAtualizado)

verificaVitoria :: Tabuleiro -> Marcacao -> Bool
verificaVitoria tab jog = do
    -- linhas
    let l1 = [tab ! (1,i) | i <- [1..3]]
    let l2 = [tab ! (2,i) | i <- [1..3]]
    let l3 = [tab ! (3,i) | i <- [1..3]]
    -- colunas
    let c1 = [tab ! (i,1) | i <- [1..3]]
    let c2 = [tab ! (i,2) | i <- [1..3]]
    let c3 = [tab ! (i,3) | i <- [1..3]]
    -- diagonal principal
    let dp = [tab ! (i,i) | i <- [1..3]]
    -- diagonal secundária
    let ds = [tab ! (i,3+1-i) | i <- [1..3]]
    
    -- linhas
    if verificaDirecao l1 jog || verificaDirecao l2 jog || verificaDirecao l3 jog then True 
    -- colunas
    else if verificaDirecao c1 jog || verificaDirecao c2 jog || verificaDirecao c3 jog then True
    -- diagonal principal
    else if verificaDirecao dp jog then True
    -- diagonal secundária
    else if verificaDirecao ds jog then True 
    -- não houve vitória
    else False

deuVelha :: Tabuleiro -> Bool
deuVelha tab = do
    let vetor = elems tab
    not (elem vazio vetor)

oponente :: Marcacao -> Marcacao
oponente jogador 
    | jogador == primeiroJogador  = segundoJogador
    | otherwise                   = primeiroJogador

-- fim de auxiliares turnoJogador

-- auxiliares de jogadorJoga

menu :: IO ()
menu = do
    putStrLn "\nmenu 1. Ganhar: se voce tem duas pecas numa linha, ponha a terceira."
    putStrLn "menu 2. Bloquear: se o oponente tiver duas pecas em linha."
    putStrLn "menu 3. Triangulo: crie uma oportunidade para ganhar de duas maneiras."
    putStrLn "menu 41. Bloquear o Triangulo do oponente colocando 2 pecas em linha."
    putStrLn "menu 42. bloquear formacao de Triangulo do oponente."
    putStrLn "menu 5. Jogue no centro.\n"

processaEntrada :: Marcacao -> Tabuleiro -> IO Posicao
processaEntrada jogador tab = do
    
    putStrLn ("Joga " ++ jogador ++ " [lin col] ou [menu X]: ")
    entrada <- getLine
    
    if ehPosicaoValida entrada then do 
        let posicao = recuperaPosicaoValida entrada
        if ehPosicaoVazia posicao tab then
            return (posicao)
        else do
            putStrLn "   Inválido: Espaço ocupado!"
            processaEntrada jogador tab
    else do 
        case entrada of
            "menu 1"      ->  return (menu1)
            "menu 2"      ->  return (menu2)
            "menu 3"      ->  return (menu3)
            "menu 41"     ->  return (menu41)
            "menu 42"     ->  return (menu42)
            "menu 5"      ->  return (menu5)
            otherwise     -> do
                putStrLn "   INVÁLIDO! Padrão: [lin col] ou [menu X]"
                putStrLn "      1 1\n      2 3\n      menu 5"
                processaEntrada jogador tab

-- fim auxiliares de jogadorJoga

-- auxiliares de processaEntrada

ehPosicaoValida :: String -> Bool
ehPosicaoValida [] = False
ehPosicaoValida entrada = do
    if (entrada !! 0 == '1' || entrada !! 0 == '2' || entrada !! 0 == '3') &&
       (entrada !! 2 == '1' || entrada !! 2 == '2' || entrada !! 2 == '3') &&
       (entrada !! 1 == ' ') then True else False

recuperaPosicaoValida :: String -> Posicao
recuperaPosicaoValida entrada = do
    let linha = if entrada !! 0 == '1' then 1 else if entrada !! 0 == '2' then 2 else 3
    let coluna = if entrada !! 2 == '1' then 1 else if entrada !! 2 == '2' then 2 else 3 
    (linha, coluna)

ehPosicaoVazia :: Posicao -> Tabuleiro -> Bool
ehPosicaoVazia pos tab = if tab ! pos == vazio then True else False

-- fim auxiliares de processaEntrada

-- auxiliares de verificaVitoria

verificaDirecao :: [Marcacao] -> Marcacao -> Bool
verificaDirecao lista jog = 
    if elem vazio lista || elem (oponente jog) lista then False else True

-- fim auxiliares de verificaVitoria

-- menus

menu1  :: Posicao
menu1 = (2, 2)
menu2  :: Posicao
menu2 = (2, 2)
menu3  :: Posicao
menu3 = (2, 2)
menu41  :: Posicao
menu41 = (2, 2)
menu42  :: Posicao
menu42 = (2, 2)
menu5  :: Posicao
menu5 = (2, 2)

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

menu :: IO ()
menu = do
    putStrLn "\nmenu 1. Ganhar: se voce tem duas pecas numa linha, ponha a terceira."
    putStrLn "menu 2. Bloquear: se o oponente tiver duas pecas em linha."
    putStrLn "menu 3. Triangulo: crie uma oportunidade para ganhar de duas maneiras."
    putStrLn "menu 41. Bloquear o Triangulo do oponente colocando 2 pecas em linha."
    putStrLn "menu 42. bloquear formacao de Triangulo do oponente."
    putStrLn "menu 5. Jogue no centro.\n"

imprimeTabuleiro :: Tabuleiro -> IO ()
imprimeTabuleiro tab = do
    putStrLn "--- JOGO DA VELHA ---\n"
    putStr "    1   2   3"
    putStrLn ("\n1   " ++ tab ! (1,1) ++ "   " ++ tab ! (1,2) ++ "   " ++ tab ! (1,3)
           ++ "\n2   " ++ tab ! (2,1) ++ "   " ++ tab ! (2,2) ++ "   " ++ tab ! (2,3)
           ++ "\n3   " ++ tab ! (3,1) ++ "   " ++ tab ! (3,2) ++ "   " ++ tab ! (3,3))

jogadorDaVez :: String -> String
jogadorDaVez jogador 
    | jogador == primeiroJogador  = segundoJogador
    | otherwise        = primeiroJogador

menu1  :: IO Posicao
menu1 = return ((-1, -1))
menu2  :: IO Posicao
menu2 = return ((-1, -1))
menu3  :: IO Posicao
menu3 = return ((-1, -1))
menu41  :: IO Posicao
menu41 = return ((-1, -1))
menu42  :: IO Posicao
menu42 = return ((-1, -1))
menu5  :: IO Posicao
menu5 = return ((-1, -1))

ehPosicaoValida :: String -> Bool
ehPosicaoValida [] = False
ehPosicaoValida entrada = do
    if (entrada !! 0 == '1' || entrada !! 0 == '2' || entrada !! 0 == '3') &&
       (entrada !! 2 == '1' || entrada !! 2 == '2' || entrada !! 2 == '3') &&
       (entrada !! 1 == ' ') then True else False

recuperaPosicaoValida :: String -> IO Posicao
recuperaPosicaoValida entrada = do
    let linha = if entrada !! 0 == '1' then 1 else if entrada !! 0 == '2' then 2 else 3
    let coluna = if entrada !! 2 == '1' then 1 else if entrada !! 2 == '2' then 2 else 3 
    return ((linha, coluna))

processaEntrada :: Marcacao -> IO Posicao
processaEntrada jogador = do
    
    putStrLn ("Joga " ++ jogador ++ " [lin col] ou [menu X]: ")
    
    entrada <- getLine
    
    if ehPosicaoValida entrada then recuperaPosicaoValida entrada
    else do 
        case entrada of
            "menu 1"      ->  menu1
            "menu 2"      ->  menu2
            "menu 3"      ->  menu3
            "menu 41"     ->  menu41
            "menu 42"     ->  menu42
            "menu 5"      ->  menu5
            otherwise     -> do
                putStrLn "   INVÁLIDO! Padrao: [lin col] ou [menu X]"
                putStrLn "      1 1\n      2 3\n      menu 5"
                processaEntrada jogador

jogadorJoga :: Tabuleiro -> Marcacao -> IO Tabuleiro
jogadorJoga tab jogador = do
    menu
    posicao <- processaEntrada jogador
    print posicao -- teste
    -- atualizaTabuleiro tab posicao jogador
    return (tab)

turnoJogador :: Tabuleiro -> Marcacao -> IO String 
turnoJogador tab jogador = do
    imprimeTabuleiro tab
    tabAtualizado <- jogadorJoga tab jogador
    return ("Teste OK?") -- teste
    {- let vencedor = verificaVitoria tabAtualizado jogador
    if vencedor then do
        return("Parabens, jogador " ++ jogador ++ "! Voce venceu!")
    else do
        if deuVelha tabAtualizado then do
            return ("Deu velha!")
        else
            turnoJogador (tabAtualizado (jogadorDaVez jogador))
    -}


main = do
    instrucoes
    -- tabuleiro, índices iniciam em (1,1) e terminam em (3,3)
    let tabuleiro = array ((1,1),(3,3)) [((x,y), vazio) | x <- [1,2,3], y <- [1,2,3]]
    resultado <- turnoJogador tabuleiro primeiroJogador
    putStrLn resultado

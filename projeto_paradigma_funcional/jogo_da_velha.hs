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

-- ajuda: https://gist.github.com/marcoscastro/d6b0be7f6cf600fdb53b

import System.IO
import System.IO.Error
import System.Process
import Data.Array


-- constantes 
vazio :: String
vazio = "-"

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

imprimeTabuleiro :: Array (Int, Int) String -> IO ()
imprimeTabuleiro tab = do
    putStrLn "--- JOGO DA VELHA ---\n"
    putStr "    1   2   3"
    putStrLn ("\n1   " ++ tab ! (1,1) ++ "   " ++ tab ! (1,2) ++ "   " ++ tab ! (1,3)
           ++ "\n2   " ++ tab ! (2,1) ++ "   " ++ tab ! (2,2) ++ "   " ++ tab ! (2,3)
           ++ "\n3   " ++ tab ! (3,1) ++ "   " ++ tab ! (3,2) ++ "   " ++ tab ! (3,3))

jogadorDaVez :: String -> String
jogadorDaVez jogador 
    | "X" = "O"
    | otherwise "X"

menu1  :: IO ()
menu1 = do print "Em breve"
menu2  :: IO ()
menu2 = do print "Em breve"
menu3  :: IO ()
menu4 = do print "Em breve"
menu41 :: IO ()
menu41 = do print "Em breve"
menu42 :: IO ()
menu42 = do print "Em breve"
menu5  :: IO ()
menu5 = do print "Em breve"

ehPosicaoValida :: String -> Bool
ehPosicaoValida entrada = do
    -- pesquisar como acessar o elemento de uma string 
    {-
      if ((entrada[0] == '1' || entrada[0] == '2' || entrada[0] == '3') &&
        (entrada[2] == '1' || entrada[2] == '2' || entrada[2] == '3') &&
        (entrada[1] == ' '))
     -}

recuperaPosicaoValida :: String -> (Int, Int)
recuperaPosicaoValida entrada = do
    {-
      if (entrada[0] == '1') posicao.linha = 0;
		else if (entrada[0] == '2') posicao.linha = 1;
		else if (entrada[0] == '3') posicao.linha = 2;
		if (entrada[2] == '1') posicao.coluna = 0;
		else if (entrada[2] == '2') posicao.coluna = 1;
		else if (entrada[2] == '3') posicao.coluna = 2;
     -}

processaEntrada :: String -> (Int, Int)
processaEntrada jogador = do
    putStr "Joga " ++ jogador ++ "[lin col] ou [menu X]: "
    let entrada <- getLine
    case () of
    _   | entrada == "menu 1"      ->  menu1
        | entrada == "menu 2"      ->  menu2
        | entrada == "menu 3"      ->  menu3
        | entrada == "menu 41"     ->  menu41
        | entrada == "menu 42"     ->  menu42
        | entrada == "menu 5"      ->  menu5
        | ehPosicaoValida entrada  ->  recuperaPosicaoValida entrada
        | otherwise  -> do
            putStr "   Padrao: [lin col] ou [menu X]"
            putStr "      1 1\n      2 3\n      menu 5"
            processaEntrada jogador

jogadorJoga :: Array (Int, Int) String -> String -> Array (Int, Int) String
jogadorJoga tab jogador = do
    menu
    let posicao = processaEntrada jogador
    atualizaTabuleiro tab posicao jogador

turnoJogador :: Array (Int, Int) String -> String -> String
turnoJogador tab jogador = do
    imprimeTabuleiro tab
    let tabAtualizado = jogadorJoga tab jogador
    let vencedor = verificaVitoria tabAtualizado jogador
    if vencedor then 
        "Parabens, jogador " ++ jogador ++ "! Voce venceu!"
    else do
        if deuVelha tabAtualizado then
            "Deu velha!"
        else
            turnoJogador (tabAtualizado (jogadorDaVez jogador))


main = do
    instrucoes
    -- tabuleiro, índices iniciam em [1 1] tamanho 3x3
    let tabuleiro = array ((1,1),(3,3)) [((x,y), vazio) | x <- [1,2,3], y <- [1,2,3]]
    -- imprimeTabuleiro (elems tabuleiro)
    print (turnoJogador (tabuleiro (jogadorDaVez "X")))
    -- imprimeTabuleiro tabuleiro
    

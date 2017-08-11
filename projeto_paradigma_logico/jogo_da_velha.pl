/* Tema 8: Jogo da Velha
 *
 * O jogo consiste de um tabuleiro, sendo este uma matriz de três linha e três colunas.
 * A seguir, a especificação do jogo:
 * a. São 2 participantes.
 * b. Um começa o jogo.
 * c. Um joga após o outro, até o fim do jogo.
 * d. Em cada jogada é colocado um símbolo ("O" ou "X") em uma posição não ocupada do tabuleiro.
 * e. No início do jogo, todas as posições do tabuleiro estão livres ou desocupadas.
 * f. O jogo termina quando se forma uma sequência de três símbolos do mesmo tipo em 
 *   linha horizontal, vertical ou diagonal. Quando isto acontece, o jogador que colocou 
 *   o símbolo ganha o jogo (e o outro perde)
 * g. O jogo também pode terminar quando alguém preenche o último espaço disponível 
 *   (neste caso quem ganha é a "velha").
 * 
 * A lógica do jogo é muito simples, de modo que não é difícil deduzir ou decorar todas 
 * as possibilidades para efetuar a melhor jogada - apesar de o número total de possibilidades ser 
 * muito grande, a maioria delas é simétrica, além de que as regras são simples.
 * 
 * Implemente as seguintes regras por ordem de prioridade:
 * 1. Ganhar: se você tem duas peças numa linha, ponha a terceira.
 * 2. Bloquear: se o oponente tiver duas peças em linha, ponha a terceira para bloqueá-lo.
 * 3. Triângulo: crie uma oportunidade em que você poderá ganhar de duas maneiras.
 * 4. Bloquear o Triângulo do oponente:
 * * Opção 1: crie 2 peças em linha para forçar o oponente a se defender, 
 *   contanto que não resulte nele criando um triângulo ou vencendo. 
 *   Por exemplo, se 'X' tem dois cantos opostos do tabuleiro e 'O' tem o centro, 'O' 
 *   não pode jogar num canto (jogar no canto nesse cenário criaria um triângulo em que 'X' vence).
 * * Opção 2: se existe uma configuração em que o oponente pode formar um triângulo, bloqueiem-no.
 * 5. Centro: jogue no centro. 
*/

tabuleiroVazio([['1', '2', '3'], 
				['4', '5', '6'], 
				['7', '8', '9']]).

/* --------------------------------------- list --------------------------------------- */
replace(L, P, E, R) :- 
    P1 is (P - 1), 
    findall(X, (nth0(I,L,Y), (I == P1 -> X = E ; X = Y)), R).
/* --------------------------------------- list --------------------------------------- */

/* --------------------------------------- mostrar tabuleiro e menu --------------------------------------- */

imprimeLinha([]).
imprimeLinha([A|B]) :- write(A), write('   '), imprimeLinha(B).

imprimeTabuleiro(_, []).
imprimeTabuleiro(N, [A|B]) :- 
	write(N), write('   '),
	imprimeLinha(A), writeln(''), 
	N1 is N + 1, 
	imprimeTabuleiro(N1, B).

imprimeMenu :- 
	writeln("\nmenu 1. Ganhar: se voce tem duas pecas numa linha, ponha a terceira."),
	writeln("menu 2. Bloquear: se o oponente tiver duas pecas em linha."),
	writeln("menu 3. Triangulo: crie uma oportunidade para ganhar de duas maneiras."),
	writeln("menu 41. Bloquear o Triangulo do oponente colocando 2 pecas em linha."),
	writeln("menu 42. bloquear formacao de Triangulo do oponente."),
	writeln("menu 5. Jogue no centro.\n").

imprimeJogoDaVelha(T) :- 
	writeln('--- JOGO DA VELHA ---\n'),
	writeln('    1   2   3'),
	imprimeTabuleiro(1, T).

/* --------------------------------------- mostrar tabuleiro e menu --------------------------------------- */

/* --------------------------------------- jogador --------------------------------------- */
oponente(Jogador, Oponente) :- Jogador = 'X', Oponente = 'O'.
oponente(Jogador, Oponente) :- Jogador = 'O', Oponente = 'X'.

processaEntrada("menu 1", Jogador, Tabuleiro, TabAtualizado) :- 
	writeln("Menu 1"),
	TabAtualizado = Tabuleiro.

processaEntrada("menu 2", Jogador, Tabuleiro, TabAtualizado) :- 
	writeln("Menu 2"),
	TabAtualizado = Tabuleiro.

processaEntrada("menu 3", Jogador, Tabuleiro, TabAtualizado) :- 
	writeln("Menu 3"),
	TabAtualizado = Tabuleiro.

processaEntrada("menu 4", Jogador, Tabuleiro, TabAtualizado) :- 
	writeln("Menu 4"),
	TabAtualizado = Tabuleiro.

processaEntrada("menu 5", Jogador, Tabuleiro, TabAtualizado) :- 
	writeln("Menu 5"),
	TabAtualizado = Tabuleiro.

processaEntrada([Linha,Coluna|[]], Jogador, Tabuleiro, TabAtualizado) :- 
	TabAtualizado = Tabuleiro.

processaEntrada(_, Jogador, Tabuleiro, TabAtualizado) :- 
	writeln("   INVÁLIDO! Padrão: [lin,col]. ou [menu X]."),
	writeln("      [1,1].\n      [2,3].\n      \"menu 5\"."),
	processaEntrada(Jogador, Tabuleiro, TabAtualizado).

processaEntrada(Jogador, Tabuleiro, TabAtualizado) :- 
	write("Joga "), write(Jogador), write(" [lin col] ou [menu X]: "),
	read(Entrada),
	processaEntrada(Entrada, Jogador, Tabuleiro, TabAtualizado).

jogadorJoga(Tabuleiro, Jogador, TabAtualizado) :- 
	imprimeMenu,
	processaEntrada(Jogador, Tabuleiro, TabAtualizado).

turnoJogador(Tabuleiro, Jogador, Resultado) :- 
	imprimeJogoDaVelha(Tabuleiro),
	jogadorJoga(Tabuleiro, Jogador, TabAtualizado),
% teste abaixo
	string_concat("Teste com ", Jogador, Resultado),
	imprimeJogoDaVelha(TabAtualizado).
%	let vencedor = verificaVitoria tabAtualizado jogador
%	if vencedor then do
%		imprimeTabuleiro tabAtualizado
%		return("Parabens, jogador " ++ jogador ++ "! Voce venceu!")
%	else do
%		if deuVelha tabAtualizado then do
%			imprimeTabuleiro tabAtualizado
%			return ("Deu velha!")
%		else
%			turnoJogador tabAtualizado (oponente jogador)
%	string_concat("Teste com ", Jogador, Resultado).
/* --------------------------------------- jogador --------------------------------------- */

:- initialization(main).

main :-
  tabuleiroVazio(T),
  turnoJogador(T, 'X', R),
  writeln(R),
  halt(0).

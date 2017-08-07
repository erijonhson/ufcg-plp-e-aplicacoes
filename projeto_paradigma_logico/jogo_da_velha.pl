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

tabuleiroVazio([['-', '-', '-'], 
				['-', '-', '-'], 
				['-', '-', '-']]).

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
	imprimeTabuleiro(1, T),
	imprimeMenu.

/* --------------------------------------- mostrar tabuleiro e menu --------------------------------------- */

:- initialization(main).

main :-
  repeat,
  tabuleiroVazio(T),
  imprimeJogoDaVelha(T),
  halt(0).

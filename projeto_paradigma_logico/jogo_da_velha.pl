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

/* --------------------------------------- list --------------------------------------- */
% http://www.swi-prolog.org/pldoc/doc_for?object=append/3

replace(List, Position, Element, Result) :- 
	findall(X, (nth1(I,List,Y), (I == Position -> X = Element ; X = Y)), Result).

columnN([],_,[]).
columnN([H|T], I, [R|X]) :- 
	nth1(I, H, R),
	columnN(T,I,X).

colunas(4, _, _).
colunas(Index, Tabuleiro, Result) :- 
	columnN(Tabuleiro, Index, Col),
	nth1(Index, Result, Col, _),
	Pos is Index + 1,
	colunas(Pos, Tabuleiro, Result).

colunas(Tabuleiro, Result) :- 
	colunas(1, Tabuleiro, Temp),
	append(Temp, [], Result).

principal([], _,[]).
principal([H|T], I, [R|X]) :- 
	nth1(I, H, R),
	P is I + 1, 
	principal(T, P, X).

secundaria([], _, []).
secundaria([H|T], I, [R|X]) :- 
	nth1(I, H, R),
	P is I - 1, 
	secundaria(T, P, X).

diagonais(Tabuleiro, Result) :- 
	principal(Tabuleiro, 1, Prin),
	secundaria(Tabuleiro, 3, Sec),
	nth1(1, Temp, Prin, []),
	nth1(2, Result, Sec, Temp).

%	Insert Element in Position of List:
%	nth1(Pos, Result, Element, List).

/* --------------------------------------- list --------------------------------------- */

/* --------------------------------------- mostrar tabuleiro e menu --------------------------------------- */

imprimeLinha([]).
imprimeLinha([H|T]) :- write(H), write('   '), imprimeLinha(T).

imprimeTabuleiro(_, []).
imprimeTabuleiro(N, [H|T]) :- 
	write(N), write('   '),
	imprimeLinha(H), writeln(''), 
	N1 is N + 1, 
	imprimeTabuleiro(N1, T).

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

situacao('X', 'X').
situacao('O', 'O').
situacao('V', 'V').
situacao(_, "RunBarryRun").

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

espacoOcupado(Linha, Coluna, Jogador, Tabuleiro, TabAtualizado) :- 
	nth1(Linha, Tabuleiro, Lista),
	nth1(Coluna, Lista, Elemento),
	Elemento \= '-',
	writeln("    Espaco Ocupado!"),
	processaEntrada(Jogador, Tabuleiro, TabAtualizado).

%Aqui será caso a jogada for valida
processaEntrada([Linha, Coluna|[]], Jogador, Tabuleiro, TabAtualizado) :- 
	espacoOcupado(Linha, Coluna, Jogador, Tabuleiro, TabAtualizado);
	nth1(Linha, Tabuleiro, Lista),
	replace(Lista, Coluna, Jogador, NovaLinha), 
	replace(Tabuleiro, Linha, NovaLinha, TabAtualizado).

processaEntrada(_, Jogador, Tabuleiro, TabAtualizado) :- 
	writeln("   INVÁLIDO! Padrão: [lin, col]. ou [menu X]."),
	writeln("      [1, 1].\n      [2, 3].\n      \"menu 5\"."),
	processaEntrada(Jogador, Tabuleiro, TabAtualizado).

processaEntrada(Jogador, Tabuleiro, TabAtualizado) :- 
	write("Joga "), write(Jogador), write(" [lin,col]. ou \"menu X\".: "),
	read(Entrada),
	processaEntrada(Entrada, Jogador, Tabuleiro, TabAtualizado).

jogadorJoga(Tabuleiro, Jogador, TabAtualizado) :- 
	imprimeMenu,
	processaEntrada(Jogador, Tabuleiro, TabAtualizado).

direcoes(Tabuleiro, Direcoes) :- 
	colunas(Tabuleiro, Colunas),
	append(Tabuleiro, Colunas, Temp),
	diagonais(Tabuleiro, Diagonais),
	append(Temp, Diagonais, Direcoes).

empate([], 'V').
empate([[C1,C2,C3]|T], Result) :- 
	C1 \= '-', C2 \= '-', C3 \= '-', empate(T, Result).

vitoria(C1, C2, C3, Jogador, Result) :- 
	C1 == Jogador, C2 == Jogador, C3 == Jogador, Result = Jogador.

verificaVitoria([], _, _, false).
verificaVitoria(_, _, 'X', 'X').
verificaVitoria(_, _, 'O', 'O').
verificaVitoria([[C1,C2,C3]|T], Jogador, Situacao, Result) :- 
	vitoria(C1, C2, C3, Jogador, Temp), Result = Temp;
	verificaVitoria(T, Jogador, "RunBarryRun", Result).

verificaVitoria(Tabuleiro, Jogador, Result) :- 
	empate(Tabuleiro, Result);
	verificaVitoria(Tabuleiro, Jogador, "RunBarryRun", Result).

turnoJogador(Tabuleiro, _, 'X', "Parabens, jogador X! Voce venceu!") :- 
	imprimeJogoDaVelha(Tabuleiro).
turnoJogador(Tabuleiro, _, 'O', "Parabens, jogador O! Voce venceu!") :- 
	imprimeJogoDaVelha(Tabuleiro). 
turnoJogador(Tabuleiro, _, 'V', "Deu velha!") :- 
	imprimeJogoDaVelha(Tabuleiro).

turnoJogador(Tabuleiro, Jogador, _, Resultado) :- 
	imprimeJogoDaVelha(Tabuleiro),
	jogadorJoga(Tabuleiro, Jogador, TabAtualizado),
	direcoes(TabAtualizado, Direcoes),''
	verificaVitoria(Direcoes, Jogador, Temp),
	situacao(Temp, Situacao),
	oponente(Jogador, Oponente),
	turnoJogador(TabAtualizado, Oponente, Situacao, Resultado).

posicoesVitoriaProximaJogada(Jogador, Tabuleiro, TabAtualizado) :-
	direcoes(Tabuleiro, Direcoes),
	verificaVitoria(Direcoes, Jogador, Temp),



/* --------------------------------------- jogador --------------------------------------- */

 :- initialization(main).

main :-
  tabuleiroVazio(Tabuleiro),
  turnoJogador(Tabuleiro, 'X', "RunBarryRun", Resultado),
  writeln(Resultado),
  halt(0).

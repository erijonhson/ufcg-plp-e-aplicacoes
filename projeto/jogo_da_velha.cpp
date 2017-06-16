/* Tema 8: Jogo da Velha
 * 
 * O jogo consiste de um tabuleiro, sendo este uma matriz de três linha e três colunas.
 * A seguir, a especificação do jogo:
 * a. São 2 participantes.
 * b. Um começa o jogo.
 * c. Um joga após o outro, até o fim do jogo.
 * d. Em cada jogada é colocado um símbolo ("O" ou "X") em uma posição não ocupada do tabuleiro.
 * e. No início do jogo, todas as posições do tabuleiro estão livres/desocupadas.
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

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "solucao_jogo_da_velha.hpp"


// variáveis globais são marcantes na programação estruturada
char tabuleiro[TAM][TAM] = 
	{
		VAZIO, VAZIO, VAZIO,
		VAZIO, VAZIO, VAZIO,
		VAZIO, VAZIO, VAZIO
	};

void imprime_menu();

void imprime_tabuleiro();

char turno(char jogador);

void jogador_joga(char jogador);

Posicao processa_entrada(char jogador);

char verifica_vitoria(char jogador);

int main() {
	
	char vencedor = CHAR_FALSO;
	
	imprime_tabuleiro();
	
	turno(JOGADOR_X);
	
	for (int i=0; i < TAM * TAM - 1; i+=2) {
		vencedor = turno(JOGADOR_O);
		if (vencedor) break;
		vencedor = turno(JOGADOR_X);
		if (vencedor) break;
	}
	
	if (vencedor) {
		printf("Parabens %c voce venceu!\n", vencedor);
	} else {
		puts("Deu velha!");
	}
	
}

void imprime_tabuleiro() {
	
	puts("--- JOGO DA VELHA ---\n");
	printf("%4c %3c %3c \n", '1', '2', '3');
	for(int i=0; i < TAM; i++) {
		printf("%d", i+1);
		for(int j=0; j < TAM; j++) {
			printf("%3c ", tabuleiro[i][j]);
		}
		printf("\n");
	}
	
}

char turno(char jogador) {
	jogador_joga(jogador);
	char vencedor = verifica_vitoria(jogador);
	imprime_tabuleiro();
	return vencedor;
}

void jogador_joga(char jogador) {
	
	int linha, coluna;
	
	imprime_menu();
	
	do {
		Posicao posicao = processa_entrada(jogador);
		if (posicao.linha != -1) { 
			linha = posicao.linha;
			coluna = posicao.coluna;
			if (tabuleiro[linha][coluna] != VAZIO) {
				printf("   Invalido: Espaco ocupado!\n");
			}
		} else {
			puts("   Menu momentaneamente invalido.");
		}
	} while(linha < 0 || linha > 2 || 
			coluna < 0 || coluna > 2 || 
			tabuleiro[linha][coluna] != VAZIO);
	
	tabuleiro[linha][coluna] = jogador;
	
}

char verifica_vitoria(char jogador) {
	
	// linhas
	for(int i=0; i < TAM; i++) {
		char vencedor = jogador;
		for(int j=0; j < TAM; j++) {
			if (tabuleiro[i][j] != jogador) {
				vencedor = CHAR_FALSO;
				break; 
			}
		}
		if (vencedor) return vencedor;
	}
	
	// colunas
	for(int i=0; i < TAM; i++) {
		char vencedor = jogador;
		for(int j=0; j < TAM; j++) {
			if (tabuleiro[j][i] != jogador) {
				vencedor = CHAR_FALSO;
				break; 
			}
		}
		if (vencedor) return vencedor;
	}
	
	// diagonal principal
	bool vitorioso = true;
	for(int i=0; i < TAM; i++) {
		if (tabuleiro[i][i] != jogador) {
			vitorioso = false;
			break;
		}
	}
	
	if (vitorioso)
		return jogador;
	
	// diagonal secundária
	vitorioso = true;
	for(int i=0; i < TAM; i++) {
		for(int j=0; j < TAM; j++) {
			if ((i+j) != (TAM - 1))
				continue;
			if (tabuleiro[i][j] != jogador) {
				vitorioso = false;
				break;
			}
		}
		if (!vitorioso)
			break;
	}
	
	if (vitorioso)
		return jogador;
	
	return CHAR_FALSO;
}

void imprime_menu() {
	puts("\nmenu 1. Ganhar: se voce tem duas pecas numa linha, ponha a terceira.");
	puts("menu 2. Bloquear: se o oponente tiver duas pecas em linha.");
	puts("menu 3. Triangulo: crie uma oportunidade para ganhar de duas maneiras.");
	puts("menu 41. Bloquear o Triangulo do oponente colocando 2 pecas em linha.");
	puts("menu 42. bloquear formacao de Triangulo do oponente.");
	puts("menu 5. Jogue no centro.\n");
}

/* Processa entrada, chama menu adequado ou recupera linha e coluna indicados.
 * Retorna posição (-1, -1) em caso de erro.
 * */
Posicao processa_entrada(char jogador) {
	
	Posicao posicao = {-1, -1};
	char entrada[10];

	printf("Joga %c [lin col] ou [menu X]: ", jogador);
	scanf(" %[^\n]s", entrada);
	
	// chamar menu 1
	if (strcmp("menu 1", entrada) == 0) {
		
		posicao = ganhar(tabuleiro, jogador);
		
	}
	// menus 2, 3 e 4 serão parecidos com menu 1
	
	// chamar menu 3
	else if (strcmp("menu 3", entrada) == 0) {
		
		posicao = triangulo(tabuleiro, jogador);
		
	}
	// chamar menu 2
	else if (strcmp("menu 5", entrada) == 0) {
		
		bool conseguiu = jogar_no_centro(tabuleiro, jogador);
		if (!conseguiu) {
			posicao.linha = 1;
			posicao.coluna = 1;
		}
		
	// jogou linha e coluna
	} else if ((entrada[0] == '1' || entrada[0] == '2' || entrada[0] == '3') &&
				(entrada[2] == '1' || entrada[2] == '2' || entrada[2] == '3') &&
				(entrada[1] == ' ')) {
		
		if (entrada[0] == '1') posicao.linha = 0;
		else if (entrada[0] == '2') posicao.linha = 1;
		else if (entrada[0] == '3') posicao.linha = 2;
		if (entrada[2] == '1') posicao.coluna = 0;
		else if (entrada[2] == '2') posicao.coluna = 1;
		else if (entrada[2] == '3') posicao.coluna = 2;
		
	// jogou errado
	} else {
		puts("   Padrao: [lin col] ou [menu X]");
		puts("      1 1\n      2 3\n      menu 5");
	}
	
	return posicao;
}

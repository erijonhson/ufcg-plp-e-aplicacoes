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

#define TAM 3
#define CHAR_FALSO '\0'

#define JOGADOR_X 'X'
#define JOGADOR_O 'O'
#define VAZIO '-'

// variáveis globais são marcantes na programação estruturada
char tabuleiro[TAM][TAM] = 
	{VAZIO, VAZIO, VAZIO,
	 VAZIO, VAZIO, VAZIO,
	 VAZIO, VAZIO, VAZIO
	};

char turno(char jogador);

void imprime_tabuleiro();

void jogador_joga(char jogador);

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
		printf("Deu velha!");
	}
	
}

void imprime_tabuleiro() {
	
	printf("%4c %3c %3c \n", '0', '1', '2');
	for(int i=0; i < TAM; i++) {
		printf("%d", i);
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
	
	do {
		printf("Joga %c [lin col]: ", jogador);
		scanf("%d %d", &linha, &coluna);
		if (linha < 0 || linha > 2 || coluna < 0 || coluna > 2) {
			printf("   Padrao: linha<espaco>coluna\n");
			printf("      0 0\n      1 2\n      2 0\n\n");
		} else if (tabuleiro[linha][coluna] != VAZIO) {
			printf("   Invalido: Espaco ocupado!\n");
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
	if ((tabuleiro[0][0] == jogador) &&
		(tabuleiro[0][0] == tabuleiro[1][1]) && 
		(tabuleiro[1][1] == tabuleiro[2][2])) {
			return jogador;
	}
	
	// diagonal secundária
	if ((tabuleiro[0][2] == jogador) &&
		(tabuleiro[0][2] == tabuleiro[1][1]) && 
		(tabuleiro[1][1] == tabuleiro[2][0])) {
			return jogador;
	}
	
	return CHAR_FALSO;
	
}

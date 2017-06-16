/* Implementação de módulo para resolver jogo da velha 
 * 
 * Autores: Eri Jonhson
 * 			Gabriel Maracajá
 * 			Pedro Costa
 * 			Victor Farias 
 * */

#ifdef __SOLUCAO_JOGO_DA_VELHA_HPP__
	
	// templated code, inline functions
	// - the part is compiled only when included from the .h
	
#else
	// non-templated code
	
#include "solucao_jogo_da_velha.hpp"

/* Método auxiliar para verificar quantas possibilidades o jogador tem para
 * ganhar em sua próxima jogada.
 * 
 * Retorna quantidade de possibilidades que o jogador tem para ganhar na próxima
 * jogada.
 * 
 * @param tabuleiro[3][3]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
int verifica_possibilidades(char tabuleiro[TAM][TAM], char jogador);

/* Função para verificar e fazer última jogada de jogador no 
 * jogo da velha recebido como parâmetro. 
 * 
 * Retorna Posição Válida (i,j) com última jogada para vencer adicionada 
 * (-1, -1), caso não exista jogada que faça jogador vencer. 
 * 
 * @param tabuleiro[3][3]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao ganhar(char tabuleiro[TAM][TAM], char jogador) {
	
	Posicao posicao = {1, 1};
	// procurar lugar para jogador ganhar
	// retornar posicao onde o jogador ganha ou (-1, -1)
	
	return posicao;
}

/* Função para verificar e indicar próxima jogada de jogador para obter um 
 * triângulo (oportunidade em que jogador poderá ganhar de duas maneiras). 
 * 
 * Retorna Posição Válida (i,j) com jogada para obter um triângulo ou 
 * (-1, -1), caso não exista jogada para obter um triângulo. 
 * 
 * @param tabuleiro[3][3]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao triangulo(char tabuleiro[TAM][TAM], char jogador) {
	
	Posicao posicao = {-1, -1};
	
	// varre todo o jogo da velha
	for(int i=0; i < TAM; i++) {
		for(int j=0; j < TAM; j++) {
			if (tabuleiro[i][j] == VAZIO) {
				tabuleiro[i][j] = jogador;
				int possibilidades = verifica_possibilidades(tabuleiro, jogador);
				tabuleiro[i][j] = VAZIO; // reseta jogada
				if (possibilidades >= 2) {
					posicao.linha = i;
					posicao.coluna = j;
					return posicao;
				}
			}
		}
	}
	
	return posicao;
}

bool jogador_ganhara_na_proxima_jogada(int quant_jogador, int quant_vazio) {
	return (quant_jogador == 2) && (quant_vazio == 1);
}

/* Método auxiliar para verificar quantas possibilidades o jogador tem para
 * ganhar em sua próxima jogada.
 * 
 * Retorna quantidade de possibilidades que o jogador tem para ganhar na próxima
 * jogada.
 * 
 * @param tabuleiro[3][3]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
int verifica_possibilidades(char tabuleiro[TAM][TAM], char jogador) {
	
	int quant_possibilidades = 0;
	int quant_jogador = 0;
	int quant_vazio = 0;
	
	// linhas
	for(int i=0; i < TAM; i++) {
		for(int j=0; j < TAM; j++) {
			if (tabuleiro[i][j] == jogador) {
				quant_jogador++;
			} else if (tabuleiro[i][j] == VAZIO) {
				quant_vazio++;
			}
		}
		
		if (jogador_ganhara_na_proxima_jogada(quant_jogador, quant_vazio)) {
			quant_possibilidades++;
		}
		
		quant_jogador = 0;
		quant_vazio = 0;
	}
	
	// colunas
	for(int i=0; i < TAM; i++) {
		for(int j=0; j < TAM; j++) {
			if (tabuleiro[j][i] == jogador) {
				quant_jogador++;
			} else if (tabuleiro[j][i] == VAZIO) {
				quant_vazio++;
			}
		}
		
		if (jogador_ganhara_na_proxima_jogada(quant_jogador, quant_vazio)) {
			quant_possibilidades++;
		}
		
		quant_jogador = 0;
		quant_vazio = 0;
	}
	
	// diagonal principal
	for(int i=0; i < TAM; i++) {
		if (tabuleiro[i][i] == jogador) {
			quant_jogador++;
		} else if (tabuleiro[i][i] == VAZIO) {
			quant_vazio++;
		}
	}
	
	if (jogador_ganhara_na_proxima_jogada(quant_jogador, quant_vazio)) {
		quant_possibilidades++;
	}
	
	quant_jogador = 0;
	quant_vazio = 0;
	
	// diagonal secundária
	for(int i=0; i < TAM; i++) {
		for(int j=0; j < TAM; j++) {
			if ((i+j) != (TAM - 1))
				continue;
			if (tabuleiro[i][j] == jogador) {
				quant_jogador++;
			} else if (tabuleiro[i][j] == VAZIO) {
				quant_vazio++;
			}
		}
	}
	
	if (jogador_ganhara_na_proxima_jogada(quant_jogador, quant_vazio)) {
		quant_possibilidades++;
	}
	
	return quant_possibilidades;
}

/* Função que retorna posicao central do tabuleiro.
 * 
 * Retorna (1, 1).
 * 
 * @param tabuleiro[3][3]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao jogar_no_centro(char tabuleiro[TAM][TAM], char jogador) {
	Posicao posicao = {1, 1};
	return posicao;
}

#endif

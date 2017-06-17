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
 
/* Verifica se há duas marcações de um mesmo jogador e uma marcação de vazio
 * em mesma direção, o que configura possível vitória na próxima jogada.
 * 
 * Retorna true caso exista possibilidade do jogador ganhar, false caso contrário.
 *
 * @param quant_jogador		quantas vezes jogador está marcado na direção 
 * @param quant_vazio		quantas vezes vazio está marcado na direção 
 * */
bool jogador_ganhara_na_proxima_jogada(int quant_jogador, int quant_vazio);
 
/* Método auxiliar para calcular posições para vitória de jogador
 * em sua próxima jogada.
 * 
 * Retorna vetor[TAM] de posições em que o jogador ganha na próxima jogada.
 * Última posição do vetor é marcada com (-1, -1).
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao* posicoes_para_vitoria_proxima_jogada(char tabuleiro[TAM][TAM], char jogador);
 
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
				tabuleiro[i][j] = jogador; // marca para testes
				int possibilidades = verifica_possibilidades(tabuleiro, jogador);
				tabuleiro[i][j] = VAZIO; // reseta jogada
				if (possibilidades >= 2) { // condição de triângulo
					posicao.linha = i;
					posicao.coluna = j;
					return posicao;
				}
			}
		}
	}
	
	return posicao;
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
	Posicao *posicoes = posicoes_para_vitoria_proxima_jogada(tabuleiro, jogador);
	int quantidade = 0;
	for(int i=0; i < TAM; i++) {
		if (posicoes[i].linha == -1) 
			break;
		quantidade++;
	}
	return quantidade;
}
 
/* Verifica se há duas marcações de um mesmo jogador e uma marcação de vazio
 * em mesma direção, o que configura possível vitória na próxima jogada.
 * 
 * Retorna true caso exista possibilidade do jogador ganhar, false caso contrário.
 *
 * @param quant_jogador		quantas vezes jogador está marcado na direção 
 * @param quant_vazio		quantas vezes vazio está marcado na direção 
 * */
bool jogador_ganhara_na_proxima_jogada(int quant_jogador, int quant_vazio) {
	return (quant_jogador == 2) && (quant_vazio == 1);
}
 
/* Método auxiliar para calcular posições para vitória de jogador
 * em sua próxima jogada.
 * 
 * Retorna vetor[TAM] de posições em que o jogador ganha na próxima jogada.
 * Última posição do vetor é marcada com (-1, -1).
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao* posicoes_para_vitoria_proxima_jogada(char tabuleiro[TAM][TAM], char jogador) {
	
	int index = 0;
	Posicao posicao = {-1, -1};
	Posicao *posicoes = (Posicao *) malloc(TAM * sizeof(Posicao));
	int quant_jogador = 0;
	int quant_vazio = 0;
	
	for(int i=0; i < TAM; i++) {
		posicoes[i] = posicao;
	}
	
	// linhas
	for(int i=0; i < TAM; i++) {
		for(int j=0; j < TAM; j++) {
			if (tabuleiro[i][j] == jogador) {
				quant_jogador++;
			} else if (tabuleiro[i][j] == VAZIO) {
				quant_vazio++;
				posicao.linha = i;
				posicao.coluna = j;
			}
		}
		
		if (jogador_ganhara_na_proxima_jogada(quant_jogador, quant_vazio)) {
			posicoes[index++] = posicao;
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
				posicao.linha = j;
				posicao.coluna = i;
			}
		}
		
		if (jogador_ganhara_na_proxima_jogada(quant_jogador, quant_vazio)) {
			posicoes[index++] = posicao;
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
			posicao.linha = i;
			posicao.coluna = i;
		}
	}
	
	if (jogador_ganhara_na_proxima_jogada(quant_jogador, quant_vazio)) {
			posicoes[index++] = posicao;
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
				posicao.linha = i;
				posicao.coluna = j;
			}
		}
	}
	
	if (jogador_ganhara_na_proxima_jogada(quant_jogador, quant_vazio)) {
			posicoes[index++] = posicao;
	}
 
	// garantir o contrato da função
	posicao.linha = -1;
	posicao.coluna = -1;
	posicoes[TAM - 1] = posicao;
	
	return posicoes;
}
 
#endif
 

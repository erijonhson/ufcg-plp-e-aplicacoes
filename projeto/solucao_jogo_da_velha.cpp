/* Implementação de módulo para resolver jogo da velha 
 * Autores: Eri Jonhson
 * 			Gabriel Maracajá
 * 			Pedro Costa
 * 			Victor Farias 
 * */

#include "solucao_jogo_da_velha.hpp"

// aqui nós implementamos os protótipos das funções 

/* Função para verificar e fazer última jogada de jogador no 
 * jogo da velha recebido como parâmetro. 
 * Retorna Posição Válida (i,j) com última jogada para vencer adicionada ou 
 * mesmo (-1, -1), caso não exista jogada que faça jogador vencer.
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

/* Função para fazer jogador jogar no centro do jogo da velha recebido como parâmetro. 
 * Retorna falso (0) caso não seja possível concluir a jogada.
 * 
 * @param tabuleiro[3][3]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
bool jogar_no_centro(char tabuleiro[TAM][TAM], char jogador) {
	if (tabuleiro[1][1] == VAZIO) {
		tabuleiro[1][1] = jogador;
		return true;
	} else {
		return false;
	}
}

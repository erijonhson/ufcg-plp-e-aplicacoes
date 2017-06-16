/* Cabeçalho de módulo para resolver jogo da velha 
 * 
 * Autores: Eri Jonhson
 * 			Gabriel Maracajá
 * 			Pedro Costa
 * 			Victor Farias 
 * */

#ifndef __solucao_jogo_da_velha_HPP__
#define __solucao_jogo_da_velha_HPP__

#define TAM 3
#define CHAR_FALSO '\0'

#define JOGADOR_X 'X'
#define JOGADOR_O 'O'
#define VAZIO '-'

/* Tipo de dado para representar uma posição do jogo da velha.
 * */
typedef struct _posicao_ {
	int linha;
	int coluna;
} Posicao;


/* Função para verificar e indicar última jogada para jogador ganhar no 
 * jogo da velha recebido como parâmetro. 
 * Retorna Tabuleiro de Jogo da Velha com última jogada para vencer adicionada ou 
 * mesmo tabuleiro, caso não exista jogada que faça jogador vencer.
 * 
 * @param tabuleiro[3][3]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao ganhar(char tabuleiro[TAM][TAM], char jogador);

/* Função para verificar e indicar próxima jogada de jogador para obter um 
 * triângulo (oportunidade em que jogador poderá ganhar de duas maneiras). 
 * 
 * Retorna Posição Válida (i,j) com jogada para obter um triângulo ou 
 * (-1, -1), caso não exista jogada para obter um triângulo. 
 * 
 * @param tabuleiro[3][3]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao triangulo(char tabuleiro[TAM][TAM], char jogador);

/* Função para fazer jogador jogar no centro do jogo da velha recebido como parâmetro. 
 * Retorna falso caso não seja possível concluir a jogada.
 * 
 * @param tabuleiro[3][3]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
bool jogar_no_centro(char tabuleiro[TAM][TAM], char jogador);


#include "solucao_jogo_da_velha.cpp"

#endif

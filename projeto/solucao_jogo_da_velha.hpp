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


/* Função para verificar e fazer última jogada de jogador no 
 * jogo da velha recebido como parâmetro. 
 * 
 * Retorna Posição Válida (i,j) com última jogada para vencer adicionada 
 * (-1, -1), caso não exista jogada que faça jogador vencer. 
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador				Representação do jogador da vez 
 * */
Posicao ganhar(char tabuleiro[TAM][TAM], char jogador);

/* Função para verificar e indicar jogada para jogador bloquear a
 * vitória do adversário no jogo da velha recebido como parâmetro.
 * 
 * Retorna Posição de última jogada para bloquear adversário ou 
 * (-1, -1), caso não exista jogada que realize o bloqueio.
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador				Representação do jogador da vez 
 * */
Posicao bloquear(char tabuleiro[TAM][TAM], char jogador);

/* Função para verificar e indicar próxima jogada de jogador para obter um 
 * triângulo (oportunidade em que jogador poderá ganhar de duas maneiras). 
 * 
 * Retorna Posição Válida (i,j) com jogada para obter um triângulo ou 
 * (-1, -1), caso não exista jogada para obter um triângulo. 
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao triangulo(char tabuleiro[TAM][TAM], char jogador);

/* Função para verificar e bloquear possível triângulo que o oponente de 
 * jogador possa construir. Nesse caso, o bloqueio será mediante ofensiva ao
 * oponente, caso em que este deve defender-se.
 * 
 * Retorna Posição Válida (i,j) com jogada para forçar oponente a defender-se 
 * (-1, -1), caso não exista jogada para tanto. 
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao bloquear_triangulo_com_ofensiva(char tabuleiro[TAM][TAM], char jogador);

/* Função para verificar e bloquear possível triângulo que o oponente de 
 * jogador possa construir. 
 * 
 * Retorna Posição Válida (i,j) com jogada para bloquear triângulo ou 
 * (-1, -1), caso não exista jogada para tanto. 
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao bloquear_triangulo(char tabuleiro[TAM][TAM], char jogador);

/* Função que retorna posicao central do tabuleiro.
 * 
 * Retorna posicao central do tabuleiro.
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador				Representação do jogador da vez 
 * */
Posicao jogar_no_centro(char tabuleiro[TAM][TAM], char jogador);


#include "solucao_jogo_da_velha.cpp"

#endif

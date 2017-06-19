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
 
/* Função auxiliar para verificar quantas possibilidades o jogador tem para
 * ganhar em sua próxima jogada.
 * 
 * Retorna quantidade de possibilidades que o jogador tem para ganhar na próxima
 * jogada.
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador				Representação do jogador da vez 
 * */
int verifica_possibilidades(char tabuleiro[TAM][TAM], char jogador);

/* Função auxiliar para recuperar oponente de jogador.
 * 
 * Retorna oponente de jogador.
 * 
 * @param jogador				Representação do jogador da vez 
 * */
char recupera_oponente(char jogador);

/* Verifica se jogador tem apenas uma jogada em algum dos cantos do tabuleiro.
 * 
 * Retorna posicao caso jogador tenha apenas uma jogada e no canto. 
 * (-1, -1) caso contrário. 
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador				Representação do jogador da vez 
 * */
Posicao uma_jogada_no_canto(char tabuleiro[TAM][TAM], char jogador);

/* Verifica se determinado par (int, int) representa posicao de canto do tabuleiro.
 * 
 * Retorna true caso (int, int) represente canto do tabuleiro.
 * false caso contrário.
 * 
 * @param linha da posicao
 * @param coluna da posicao
 * */
bool posicao_eh_canto(int linha, int coluna);

/* Função para verificar se a configuração atual do tabuleiro é propícia para
 * oponente conseguir criar um triângulo em suas próximas jogadas.
 * 
 * Retorna true, caso seja possível; falso, caso contrário.
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param oponente				Representação do oponente da vez 
 * */
bool oponente_consegue_triangulo(char tabuleiro[TAM][TAM], char oponente);

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
 * @param jogador				Representação do jogador da vez 
 * */
Posicao* posicoes_para_vitoria_proxima_jogada(char tabuleiro[TAM][TAM], char jogador);
 
/* Função para verificar e fazer última jogada de jogador no 
 * jogo da velha recebido como parâmetro. 
 * 
 * Retorna Posição Válida (i,j) com última jogada para vencer adicionada 
 * (-1, -1), caso não exista jogada que faça jogador vencer. 
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador				Representação do jogador da vez 
 * */
Posicao ganhar(char tabuleiro[TAM][TAM], char jogador) {
	
	Posicao posicao_vitoria = {-1,-1};
	
	Posicao *posicoes = posicoes_para_vitoria_proxima_jogada(tabuleiro, jogador);
	
	if(posicoes[0].linha !=-1){
		posicao_vitoria = posicoes[0];
	}
	
	free(posicoes);	
	
	return posicao_vitoria;
}

/* Função para verificar e indicar jogada para jogador bloquear a
 * vitória do adversário no jogo da velha recebido como parâmetro.
 * 
 * Retorna Posição para bloquear adversário ou  (-1, -1), caso não exista 
 * jogada que realize o bloqueio.
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador				Representação do jogador da vez 
 * */
Posicao bloquear(char tabuleiro[TAM][TAM], char jogador) {
	
	Posicao posicao_bloqueio = {-1,-1};
	
	char oponente = recupera_oponente(jogador);
	
	Posicao *posicoes = posicoes_para_vitoria_proxima_jogada(tabuleiro, oponente);
	
	if(posicoes[0].linha != -1) {
		posicao_bloqueio = posicoes[0];
	}
	
	free(posicoes);
	
	return posicao_bloqueio;
}
 
/* Função para verificar e indicar próxima jogada de jogador para obter um 
 * triângulo (oportunidade em que jogador poderá ganhar de duas maneiras). 
 * 
 * Retorna Posição Válida (i,j) com jogada para obter um triângulo ou 
 * (-1, -1), caso não exista jogada para obter um triângulo. 
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao triangulo(char tabuleiro[TAM][TAM], char jogador) {
	
	Posicao posicao = {-1, -1};
	
	// verifica se eh necessário defender-se
	posicao = bloquear(tabuleiro, jogador);
	if (posicao.linha != -1) {
		return posicao;
	}
	
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
Posicao bloquear_triangulo_com_ofensiva(char tabuleiro[TAM][TAM], char jogador) {
	
		// verifica se eh necessário defender-se
	Posicao posicao = bloquear(tabuleiro, jogador);
	if (posicao.linha != -1) {
		return posicao;
	}
	
	// analisa situação do oponente --------------------------------------------
	char oponente = recupera_oponente(jogador);
	
	// se oponente fez primeira jogada no canto, se defenda no centro 
	posicao = uma_jogada_no_canto(tabuleiro, oponente);
	if (posicao.linha != -1) {
		return jogar_no_centro(tabuleiro, jogador);
	}
	
	// varre todo o jogo da velha
	for(int i=0; i < TAM; i++) {
		for(int j=0; j < TAM; j++) {
			if (tabuleiro[i][j] == VAZIO) {
				tabuleiro[i][j] = jogador; // marca para testes
				Posicao *posicoes = posicoes_para_vitoria_proxima_jogada(tabuleiro, jogador);
				if (posicoes[0].linha == -1) {
					tabuleiro[i][j] = VAZIO; // reseta jogada
					free(posicoes);
					continue;
				}
				bool consegue = oponente_consegue_triangulo(tabuleiro, oponente);
				tabuleiro[i][j] = VAZIO; // reseta jogada
				if (!consegue) { // evita triângulo
					posicao.linha = i;
					posicao.coluna = j;
					return posicao;
				}
				free(posicoes);
			}
		}
	}
	
	return posicao;
}

/* Função para verificar e bloquear possível triângulo que o oponente de 
 * jogador possa construir. 
 * 
 * Retorna Posição Válida (i,j) com jogada para bloquear triângulo ou 
 * (-1, -1), caso não exista jogada para tanto. 
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador			Representação do jogador da vez 
 * */
Posicao bloquear_triangulo(char tabuleiro[TAM][TAM], char jogador) {
	
	// verifica se eh necessário defender-se
	Posicao posicao = bloquear(tabuleiro, jogador);
	if (posicao.linha != -1) {
		return posicao;
	}
	
	// analisa situação do oponente --------------------------------------------
	char oponente = recupera_oponente(jogador);
	
	// se oponente fez primeira jogada no canto, se defenda no centro 
	posicao = uma_jogada_no_canto(tabuleiro, oponente);
	if (posicao.linha != -1) {
		return jogar_no_centro(tabuleiro, jogador);
	}
	
	// varre todo o jogo da velha
	for(int i=0; i < TAM; i++) {
		for(int j=0; j < TAM; j++) {
			if (tabuleiro[i][j] == VAZIO) {
				tabuleiro[i][j] = jogador; // marca para testes
				bool consegue = oponente_consegue_triangulo(tabuleiro, oponente);
				tabuleiro[i][j] = VAZIO; // reseta jogada
				if (!consegue) { // evita triângulo
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
 * Retorna posicao central do tabuleiro.
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador				Representação do jogador da vez 
 * */
Posicao jogar_no_centro(char tabuleiro[TAM][TAM], char jogador) {
	int meio = (int) TAM / 2;
	Posicao posicao = {meio, meio};
	return posicao;
}
 
/* Função auxiliar para verificar quantas possibilidades o jogador tem para
 * ganhar em sua próxima jogada.
 * 
 * Retorna quantidade de possibilidades que o jogador tem para ganhar na próxima
 * jogada.
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador				Representação do jogador da vez 
 * */
int verifica_possibilidades(char tabuleiro[TAM][TAM], char jogador) {
	Posicao *posicoes = posicoes_para_vitoria_proxima_jogada(tabuleiro, jogador);
	int quantidade = 0;
	for(int i=0; i < TAM; i++) {
		if (posicoes[i].linha == -1) 
			break;
		quantidade++;
	}
	free(posicoes);
	return quantidade;
}

/* Função auxiliar para recuperar oponente de jogador.
 * 
 * Retorna oponente de jogador.
 * 
 * @param jogador				Representação do jogador da vez 
 * */
char recupera_oponente(char jogador) {
	return jogador == JOGADOR_O ? JOGADOR_X : JOGADOR_O;
}

/* Verifica se jogador tem apenas uma jogada em algum dos cantos do tabuleiro.
 * 
 * Retorna posicao caso jogador tenha apenas uma jogada e no canto. 
 * (-1, -1) caso contrário. 
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador				Representação do jogador da vez 
 * */
Posicao uma_jogada_no_canto(char tabuleiro[TAM][TAM], char jogador) {
	
	int quant = 0;
	Posicao posicao = {-1, -1};
	
	for(int i=0; i < TAM; i++) {
		for(int j=0; j < TAM; j++) {
			if (tabuleiro[i][j] == jogador) {
				quant++;
				if (posicao_eh_canto(i, j)) {
					posicao.linha = i;
					posicao.coluna = j;
				}
			}
		}
	}
	
	if (quant != 1) {
		posicao.linha = -1;
		posicao.coluna = -1;
	}
	
	return posicao;
}

/* Verifica se determinado par (int, int) representa posicao de canto do tabuleiro.
 * 
 * Retorna true caso (int, int) represente canto do tabuleiro.
 * false caso contrário.
 * 
 * @param linha da posicao
 * @param coluna da posicao
 * */
bool posicao_eh_canto(int linha, int coluna) {
	int beira = TAM - 1;
	return (linha == 0 && coluna == 0) ||
			(linha == 0 && coluna == beira) ||
			(linha == beira && coluna == 0) ||
			(linha == beira && coluna == beira);
}

/* Função para verificar se a configuração atual do tabuleiro é propícia para
 * oponente conseguir criar um triângulo em suas próximas jogadas.
 * 
 * Retorna true, caso seja possível; falso, caso contrário.
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param oponente				Representação do oponente da vez 
 * */
bool oponente_consegue_triangulo(char tabuleiro[TAM][TAM], char oponente) {
	
	// verifica se eh necessário defender-se
	Posicao posicao = bloquear(tabuleiro, oponente);
	if (posicao.linha != -1) {
		tabuleiro[posicao.linha][posicao.coluna] = oponente;
		int possibilidades = verifica_possibilidades(tabuleiro, oponente);
		tabuleiro[posicao.linha][posicao.coluna] = VAZIO;
		if (possibilidades >= 2)
			return true;
		else
			return false;
	}
	
	posicao = triangulo(tabuleiro, oponente);
	
	if (posicao.linha == -1)
		return false;
	else
		return true;
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
 * Obs.: Após usar o vetor retornado, desaloque-o com free().
 * 
 * @param tabuleiro[TAM][TAM]	Representação do tabuleiro 
 * @param jogador				Representação do jogador da vez 
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
 

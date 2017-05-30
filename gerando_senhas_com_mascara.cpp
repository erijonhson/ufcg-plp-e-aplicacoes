#include <stdio.h> 

/* É comum fazer uso de uma máscara para gerar senhas ou mensagens criptografadas de forma ainda mais segura. 
 * Uma máscara indica aonde num texto a informação deve ser extraída. 
 * Faça um programa que recebe quatro números e depois quatro palavras. 
 * Esses quatro números indicarão a posição do caractere relevante de cada palavra. 
 * Cada caractere relevante deve ser concatenado para gerar uma senha. 
 * Por exemplo, considere as entradas: 1, 2, 0 e 1. Considere 4 palavras de entrada: uva, ameixa, jaca, cama. 
 * Os caracteres relevantes são 'v' (posição 1 de uva), 'e' (posição 2 de ameixa), 'j' (posição 0 de java) e 'a' (posição 1 de cama).
 * Seu programa deve imprimir a seha concatenada, considerando essas 8 entradas.
 * */


int main() {
	const int LEN_STRING = 256;
	const int MAX = 4;
	int chaves[MAX];
	char palavra[LEN_STRING];
	char senha[LEN_STRING];
	
	for (int i = 0; i < MAX; i++) {
		scanf("%d", &chaves[i]);
	}
	
	for (int i = 0; i < MAX; i++) {
		scanf(" %s", palavra);
		senha[i] = palavra[chaves[i]];
	}
	
	printf("%s\n", senha);
	
	return 0;
	
}

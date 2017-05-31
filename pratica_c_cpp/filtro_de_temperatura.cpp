#include <stdio.h> 

/* Uma casa inteligente apresentava diferentes sensores de temperatura, sendo um destes sensores defeituoso. 
 * Neste sensor, as temperaturas aparecem com valor negativo, o que não acontece no local aonde a casa fica. 
 * Considerando que todos os sensores enviam seus dados para um sistema central de armazenagem de registros, 
 * faça um programa que processe esse registro indicando o número de medições com falhas (ou seja, com valores negativos). 
 * Esse registro do sistema termina com o valor 0. Veja o exemplo. 
 * */


int main() {
	
	int temperatura, falhas = 0;
	
	while(scanf("%d", &temperatura), temperatura) {
		if (temperatura < 0) {
			falhas++;
		}
	}
	
	printf("%d\n", falhas);
	
	return 0;
	
}

#include <stdio.h> 
#include <math.h>

/* Se n e n+2 são números primos, então dizem-se primos casados. 
 * Escreva um programa usando funções que receba um inteiro e determine se este é: 
 * "NAO PRIMO!", "PRIMO!" ou "PRIMO CASADO!".
 * */

bool eh_primo(int num) {
	if (num % 2 == 0)
		return false;
	for(int i = 3; i <= sqrt(num); i += 2) {
		if (num % i == 0)
			return false;
	}
	return true;
}

int main() {
	
	int num;
	
	scanf("%d", &num);
	
	if(eh_primo(num)) {
		if (eh_primo(num + 2))
			printf("PRIMO CASADO!\n");
		else
			printf("PRIMO!\n");
	} else {
		printf("NAO PRIMO!\n");
	}
	
	return 0;
	
}


#include <stdio.h> 

// note que não usei vector ou array

/* Crie um programa que leia o tamanho e um array de números inteiros. 
 * Em seguida, remova todos os elementos duplicados do array. Ao final, o array deve conter apenas valores únicos. 
 * */

bool existia_num(int vetor[], int tamanho, int num) {
	
	for(int i = 0; i < tamanho; i++) {
		if (vetor[i] == num)
			return true;
	}
	
	vetor[tamanho] = num;
	
	return false;
	
}

int main() {
	
	int tam, num;
	
	scanf("%d", &tam);
	
	int lidos[tam] = {0};
	
	for(int i = 0; i < tam; i++) {
		scanf("%d", &num);
		if (!existia_num(lidos, i, num))
			printf("%d ", num);
	}
	
	return 0;
	
}

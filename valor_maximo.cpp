#include <stdio.h> 

/* Dado quatro números inteiros m, n, o e p, determinar, entre todos os pares de números (x,y) tais que m<= x<= n e o<=y <=p, 
 * o par para o qual o valor da expressão x^2 - 3y + x seja máximo e calcular também esse máximo.
 * */

int main() {
	
	int m, n, o, p;
	
	scanf("%d %d %d %d", &m, &n, &o, &p);
	
	// como assim isso não funciona??
	int maximo = (m*m) - (3*0) + (m);
	int x_final=m, y_final=o;
	
	printf("veja: %d\n", maximo);
	
	// m <= x <= n e o <= y <=p 	
	for (int x = m; x <= n; x++) { 
		for (int y = o; y <= p; y++) {
			int temp = (x*x) - (3*y) + (x);
			if (maximo < temp) {
				maximo = temp;
				x_final = x;
				y_final = y;
			}
		}
	}
	
	printf("(%d,%d)\n", x_final, y_final);
	printf("%d\n", maximo);
	
	return 0;
	
}

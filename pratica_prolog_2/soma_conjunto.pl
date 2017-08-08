/* soma_conjunto.pl */

existe_na_lista(_, [], R) :- R = false.
existe_na_lista(X, [A|_], R) :- A = X, R = true.
existe_na_lista(X, [A|B], R) :- A =\= X, existe_na_lista(X, B, R).

lista_nao_duplicados([], A) :- A = [].
lista_nao_duplicados([A|B], Y) :- existe_na_lista(A, B, R), R = false, lista_nao_duplicados(B, B1), Y = [A|B1].
lista_nao_duplicados([A|B], Y) :- existe_na_lista(A, B, R), R = true, lista_nao_duplicados(B, B1), Y = B1.

soma_elementos([], S) :- S is 0.
soma_elementos([A|B], S) :- soma_elementos(B, S1), S is A + S1.

soma_nao_duplicados(X, S) :- lista_nao_duplicados(X, Y), soma_elementos(Y, S).

:- initialization(main).

main :-
  repeat,
  read(X),
  soma_nao_duplicados(X, S),
  write(S),nl,
  halt(0).

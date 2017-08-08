/* deleta_primeiro.pl */

remove([], _, Z) :- Z = [].
remove([A|B], Y, Z) :- A = Y, Z = B.
remove([A|B], Y, Z) :- A \= Y, remove(B, Y, B1), Z = [A|B1].

:- initialization(main).

main :-
  repeat,
  read(X), read(Y),
  remove(X, Y, Z),
  write(Z),nl,
  halt(0).

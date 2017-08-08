/* reverte.pl */

reverte([],[]).
reverte([A|B],Y) :- reverte(B,R), concatena(R,[A],Y).

concatena([],L,L).
concatena([X|L1],L2,[X|L3]) :- concatena(L1,L2,L3). 

:- initialization(main).

main:-
  read(X),
  reverte(X,Y),
  write(Y),nl,
  halt(0).

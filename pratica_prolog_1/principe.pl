/* A lista abaixo apresenta o histórico de reinado dos príncipes de Gales nos séculos 9 e 10. Os nomes estão em galês.
python dirlididi.py submit Q63fZyq12 yjeHUYkzj79d principe.pl
rhodi 844-878
anarawd 878-916
hywel_dda 916-950
lago_ap_idwal 950-979
hywal_ap_Ieuaf 979-985
cadwallow 985-986
maredudd 986-999 
*/

principe(X, rhodi) :- (X > 844), (X =< 878).
principe(X, anarawd) :- (X > 878), (X =< 916).
principe(X, hywel_dda) :- (X > 916), (X =< 950).
principe(X, lago_ap_idwal) :- (X > 950), (X =< 979).
principe(X, hywal_ap_Ieuaf) :- (X > 979), (X =< 985).
principe(X, cadwallon) :- (X > 985), (X =< 986).
principe(X, maredudd) :- (X > 986), (X =< 999).


:- initialization(main).

main :-
  repeat,
  read(X),
  principe(X, Y),
  write(Y),nl,
  halt(0).


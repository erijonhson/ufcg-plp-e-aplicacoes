/* Dada a seguinte configuração das notas de uma turma:
python dirlididi.py submit OoB154bho yjeHUYkzj79d situacao.pl

    joao: 5.0; 7.0; 8.0
    maria: 6.0; 6.0; 6.0
    joana: 8.0; 5.1; 10
    mariana: 9.0; 9.0; 3.0
    cleuza: 8.5; 7.0; 8.6
    jose: 3.5; 3.0; 2.0
    mary: 10.0; 8.0; 7.0

Escreva um programa que determine situação do aluno segundo sua média. Sabendo que:

Média de 7.0 á 10.0 = aprovado

Média de 4.0 á 6.9 = final

Média de 0.0 á 3.9 = reprovado

*/

notas(joao, 5.0, 7.0, 8.0).
notas(maria, 6.0, 6.0, 6.0).
notas(joana, 8.0, 5.1, 10).
notas(mariana, 9.0, 9.0, 3.0).
notas(cleuza, 8.5, 7.0, 8.6).
notas(jose, 3.5, 3.0, 2.0).
notas(mary, 10.0, 8.0, 7.0).

media(X, M) :- notas(X, A, B, C), M is ((A + B + C) / 3).

situacao(X, S) :- media(X, M), S = aprovado, M >= 7.0, M =< 10.0.
situacao(X, S) :- media(X, M), S = final, M >= 4.0, M < 7.
situacao(X, S) :- media(X, M), S = reprovado, M >= 0.0, M < 4.0.

:- initialization(main).

main :-
  repeat,
  read(X),
  situacao(X, Y),
  write(Y),nl,
  halt(0).


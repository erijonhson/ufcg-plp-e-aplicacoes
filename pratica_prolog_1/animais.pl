/* Expresse através de fatos e regras Prolog as informações contidas na seguinte sentença:
python dirlididi.py submit LlqiZtMDA yjeHUYkzj79d animais.pl

"joao é um pássaro. pedro é um peixe. maria é uma minhoca. 
Pássaros gostam de minhocas. Gatos gostam de peixes. Gatos gostam de pássaros. 
Amigos gostam uns dos outros. O meu gato é meu amigo. 
O meu gato come tudo o que gosta, exceto pessoas. O nome do meu gato é miau."

Implemente, entre outras, as seguinte regras: gosta(X,Y); amigo(X,Y); e come(X,Y).

*/

passaro(joao).
peixe(pedro).
minhoca(maria).
gato(miau).
pessoa(eu).

/* X gosta de Y */
gosta(X, Y) :- passaro(X), minhoca(Y).
gosta(X, Y) :- gato(X), peixe(Y).
gosta(X, Y) :- gato(X), passaro(Y).

/* X é amigo de Y */
amigo(X, Y) :- gosta(X, Y), gosta(Y, X).

/* X come Y */
come(X, Y).

/* O meu gato é meu amigo. */
gosta(miau, eu).
gosta(eu, miau).

/* O meu gato come tudo o que gosta, exceto pessoas. */
come(miau, Y) :- gosta(miau, Y), Y \= pessoa(Y).
come(X, Y) :- passaro(X), minhoca(Y).
come(X, Y) :- gato(X), peixe(Y).
come(X, Y) :- gato(X), passaro(Y).

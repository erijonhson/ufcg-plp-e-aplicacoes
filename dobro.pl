:- initialization main.
 
main:-
    repeat,
    read_line_to_codes(user_input, X3),
    string_to_atom(X3,X2),
    atom_number(X2,X),
    Y is X * 2,
    write(Y),nl,
    halt(0).

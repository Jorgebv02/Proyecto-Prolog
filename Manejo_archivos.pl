
% Lee un archivo .txt y guarda su contenido en una lista.
read_file(File, CharList) :-
	see(File),
	read_list(CharList),
	%print(CharList),
	seen.
 
%  Lee los caracteres del input hasta llegar al final (hasta alcanzar la bandera eof).
read_list([Char | List]) :-
	get0(Char),   	 % Obtiene el valor ASCII del caracter. 
	Char =\= -1, !,  % Mientras no ha llegado al final del archivo.
	%put(Char),		 % Imprime el caracter.		
	read_list(List).

read_list([]).

% Convierte cada código ASCII de una lista en el caracter equivalente.
cambiarCaracteres([Car | Lista]) :-
	(	
	Car =:= 10 -> print("\n"); % Encontró cambio de línea en ASCII.
	Car =:= 32 -> put(Car); % Encontró un espacio en ASCII.
	Car =:= 48 -> print(0);	  % Encontró un 0 en ASCII.
	Car =:= 49 -> print(1);	  % Encontró un 1 en ASCII.
	Car =:= 50 -> print(2);	  % Encontró un 2 en ASCII.
	Car =:= 51 -> print(3);	  % Encontró un 3 en ASCII.
	Car =:= 52 -> print(4);	  % Encontró un 4 en ASCII.
	Car =:= 53 -> print(5);   % Encontró un 5 en ASCII.
	Car =:= 54 -> print(6);   % Encontró un 6 en ASCII.
	Car =:= 55 -> print(7);   % Encontró un 7 en ASCII.
	Car =:= 56 -> print(8);   % Encontró un 8 en ASCII.
	Car =:= 57 -> print(9);	  % Encontró un 9 en ASCII.
	print(Car)),
	cambiarCaracteres(Lista).
cambiarCaracteres([]).

c :- 
	read_file('Prueba.txt', L),
	cambiarCaracteres(L).

conc([], L2,L2).
conc([X|R],L2,[X|Z]) :- conc(R, L2, Z). 

my_length([], L, L).

my_length([_|Xs], N, L):-
          N2 is N + 1,
          my_length(Xs, N2, L).




/*main :-
    open('Prueba.txt', read, Str),
    leersh(Str,Lines),
    close(Str),
    write(Lines), nl.

leersh(Stream,[]) :-
    at_end_of_stream(Stream).

leersh(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    leersh(Stream,L).*/

cls :- write('\e[2J'). % para limpiar la consola.
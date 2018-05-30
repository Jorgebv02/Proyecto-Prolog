% Lee un archivo .txt y guarda su contenido en una lista.
read_file(File, CharList) :-
	see(File),			 % Abre el archivo para lectura.
	read_list(CharList),	
	seen.				 % Cierra el archivo.
 
%  Lee los caracteres del input hasta llegar al final (hasta alcanzar la bandera eof).
read_list([Char | List]) :-
	get0(Char),   	 % Obtiene el valor ASCII del caracter. 
	Char =\= -1, !,  % Mientras no haya llegado al final del archivo.
	read_list(List).
read_list([]).		 % Si es una lista vacía.

%-----------------------------------------------------------------------%
% Todo lo anterior da como resultado una lista de la forma:				%
%           [49,32,50,32,51,10,49,49,32,49,51,32,50,57,32,51,49,10,...] %
% ya que guarda el valor ASCII de cada caracter leído.					%
%-----------------------------------------------------------------------%

% Recibe una lista con la siguiente estructura: 
%						["1 2 3","11 13 29 31","26 11 13 24"]
% Entonces se recorre cada string y lo separa en una lista a partir de los espacios en blanco.
% Por ejemplo: toma el "1 2 3" y lo separa a ["1", "2", "3"] y así con los demás.
recorreFilas([_|_], 1).
recorreFilas([C|Xs], N):-
	N2 is N - 1,       
	split_string(C, ' ', '', LS), % Separa el string C a partir de los espacios y guarda el resultado en la lista LS.
    print(LS),
    nl,
    recorreFilas(Xs, N2).

% Obtiene el tamaño de una lista.
tamLista([], L, L).			% Si la lista es vacía.
tamLista([_|Xs], N, L) :- 	% Si tiene elementos.
    N2 is N + 1,
    tamLista(Xs, N2, L).

main :- 
	read_file('Prueba.txt', L),		% Lee el archivo y guarda el contenido ASCII en L.
	atom_chars(SC, L),				% Toma la lista L y convierte cada ASCII a un caracter y lo guarda en SC.
	split_string(SC, '\n', '', LS),	% Separa el string SC a partir de los cambios de línea y guarda el resultado en la lista LS.
	tamLista(LS, 0, Tam),			% Guarda en Tam el tamaño de la lista LS.		
	recorreFilas(LS, Tam).

cls :- write('\e[2J'). % Es sólo para limpiar la consola.


/**********************************************
 *            MANEJO DE ARCHIVOS              *   
 **********************************************/

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
recorreFilas([], 0).
recorreFilas([C|Xs], N):-
	N2 is N - 1,       
	split_string(C, ' ', '', LS), % Separa el string C a partir de los espacios y guarda el resultado en la lista LS.
    convertirLista(LS,LRes),principal(LRes),
    nl,
    recorreFilas(Xs, N2),!.


main :- 
	read_file('Prueba.txt', L),		% Lee el archivo y guarda el contenido ASCII en L.
	atom_chars(SC, L),				% Toma la lista L y convierte cada ASCII a un caracter y lo guarda en SC.
	split_string(SC, '\n', '', LS),	% Separa el string SC a partir de los cambios de línea y guarda el resultado en la lista LS.
	largoLista(LS, 0, Tam),			% Guarda en Tam el tamaño de la lista LS.		
	recorreFilas(LS, Tam).

cls :- write('\e[2J'). % Es sólo para limpiar la consola.

/*********************************************
 *CONVIERTE UNA LISTA DE STRINGS EN INTEGER
 ********************************************/

convertirLista(Lista,ListaRes):- largoLista(Lista,0,Largo),
    convertirListaAux(Lista,Largo,ListaRes).

convertirListaAux(_,0,[]).
convertirListaAux(L,LargoLista,Resultado):-
    Pos is LargoLista - 1,
    getElem(Pos,L,Elemento), number_codes(N, Elemento),
    convertirListaAux(L, Pos, Res),
    insFinal(Res,N,Resultado),!.

/**********************************************
 *            ALGORITMOS                      *   
 **********************************************/

principal(Lista):- (esTriangulo(Lista), print(Lista),write(' Los vértices son de un triángulo válido'),!);
    (esHexagono(Lista),print(Lista),write(' Los vértices son de un hexagono válido'),!); 
    (esParalelogramo(Lista), print(Lista),write(' Los vértices son de un paralelogramo válido'),!);
    (print(Lista),write(' no son vértices de una figura válida.'),!).

/*RETORNA UNA LISTA CON LA SIGUIENTE COMBINACION DE ELEMENTOS
 * DADA LA LISTA [A,B,C,D,E,F]
 * RETORNA [[A,B],[A,F],[B,C],[C,D],[D,E],[E,F]]
 * 
 * NECESARIA PARA LOS HEXAGONOS
 * */
combinatoriaHexagono(L1,R):- getElem(0,L1,S1), getElem(1,L1,S2),
    getElem(2,L1,S3),getElem(3,L1,S4), getElem(4,L1,S5),
    getElem(5,L1,S6),insFinal([],[S1,S2],S7), insFinal(S7,[S1,S6],S8),
    insFinal(S8,[S2,S3],S9),insFinal(S9,[S3,S4],S10),
    insFinal(S10,[S4,S5],S11),insFinal(S11,[S5,S6],R).

/*RECIBE UNA LISTA DE PUNTOS DEL UNIVERSO,
 * Y RETORNA UNA LISTA CON LAS DISTANCIAS REGISTRADAS ENTRE ELLOS.
 * LOS PRIMEROS DOS ELEMENTOS REPRESENTAN LA DISTANCIA X,Y ENTRE LOS
 * PRIMEROS DOS PUNTOS, LOS SIGUIENTES DOS LA DISTANCIA X,Y ENTRE EL PUNTO A,D
 * EJEMPLO: procHexagono([4,5,9,13,12,7],L) --> 
 * L =  [1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1]*/
procHexagono(L1,L2):- combinatoriaHexagono(L1,R), distanciasPuntos(R,L2).

/*RECIBE UNA LISTA DE PUNTOS QUE PERTENECEN AL UNIVERSO,
 * E INDICA SI ES O NO ES HEXAGONO.*/
esHexagono(L):- largoLista(L,0,L1), L1=6, procHexagono(L,R1),filtra(R1,R2),
    largoLista(R2,0,M),M=2, existe(0,R2),!. 

/*RETORNA UNA LISTA CON LA SIGUIENTE COMBINACION DE ELEMENTOS
 * DADA LA LISTA [A,B,C]
 * RETORNA [[A,B],[A,C],[B,C]]
 * 
 * NECESARIA PARA LOS TRIANGULOS
 * */

combinatoriaTriangulo(L1,R):- getElem(0,L1,S1), getElem(1,L1,S2),
    getElem(2,L1,S3),insFinal([],[S1,S2],S5), insFinal(S5,[S1,S3],S6),
    insFinal(S6,[S2,S3],R).

/*RECIBE UNA LISTA DE PUNTOS DEL UNIVERSO,
 * Y RETORNA UNA LISTA CON LAS DISTANCIAS REGISTRADAS ENTRE ELLOS.
 * LOS PRIMEROS DOS ELEMENTOS REPRESENTAN LA DISTANCIA X,Y ENTRE LOS
 * PRIMEROS DOS PUNTOS, LOS SIGUIENTES DOS LA DISTANCIA X,Y ENTRE EL PUNTO A,D
 * EJEMPLO: procTriangulo([1,2,3],L) --> L = [0, 1, 1, 1, 1, 0]*/
procTriangulo(L1,L2):- combinatoriaTriangulo(L1,R), distanciasPuntos(R,L2).

/*RECIBE UNA LISTA DE PUNTOS QUE PERTENECEN AL UNIVERSO,
 * E INDICA SI ES O NO ES TRIÁNGULO.*/
esTriangulo(L):- largoLista(L,0,L1), L1=3, procTriangulo(L,R1),
    filtra(R1,R2),largoLista(R2,0,M), M=2, existe(0,R2),!. 



/*RETORNA UNA LISTA CON LA SIGUIENTE COMBINACION DE ELEMENTOS
 * DADA LA LISTA [A,B,C,D]
 * RETORNA [[A,B],[A,D],[B,C],[C,D]]
 * 
 * NECESARIA PARA LOS PARALELOGRAMOS
 * */

combinatoriaParalelogramo(L1,R):- getElem(0,L1,S1), getElem(1,L1,S2),
    getElem(2,L1,S3), getElem(3,L1,S4),
    insFinal([],[S1,S2],S5), insFinal(S5,[S1,S4],S6),
    insFinal(S6,[S2,S3],S7),insFinal(S7,[S3,S4],R).

/*RECIBE UNA LISTA DE PUNTOS DEL UNIVERSO,
 * Y RETORNA UNA LISTA CON LAS DISTANCIAS REGISTRADAS ENTRE ELLOS.
 * LOS PRIMEROS DOS ELEMENTOS REPRESENTAN LA DISTANCIA X,Y ENTRE LOS
 * PRIMEROS DOS PUNTOS, LOS SIGUIENTES DOS LA DISTANCIA X,Y ENTRE EL PUNTO A,D
 * EJEMPLO: procParalelogramo([11,13,26,24],L) --> L = [0, 2, 2, 2, 2, 2, 0, 2]*/
procParalelogramo(L1,L2):- combinatoriaParalelogramo(L1,R), distanciasPuntos(R,L2).

/*RECIBE UNA LISTA DE PUNTOS QUE PERTENECEN AL UNIVERSO,
 * E INDICA SI ES O NO ES PARALELOGRAMO.*/

esParalelogramo(L):- largoLista(L,0,L1), L1=4,procParalelogramo(L,R1),filtra(R1,R2),
    largoLista(R2,0,M),M=2, existe(0,R2),!.     

/*DADA UNA LISTA DE COMBINACIONES ENTRE PUNTOS, CALCULA LA DISTANCIA
 * ENTRE LOS PUNTOS DE CADA COMBINACIÓN, Y AGREGA ESTAS DISTANCIAS
 * A LA LISTA RESULTANTE.
 * Ejemplo: distanciasPuntos([[1,2],[2,3],[1,3]],R)--> R = [1, 1, 0, 1, 1, 0]
 * esto porque la distancia entre 1 y 2 es (0,1), entre 2 y 3 es (1,0)
 * y entre 1 y 3 es (1,1)*/

distanciasPuntos([],[]).
distanciasPuntos([X|Xs], S):-
          distanciasPuntos(Xs, S2),
          calcularDistanciaDosPuntos(X,S3,S4),
          insFinal(S2,S3,S5), insFinal(S5,S4,S).

/*
 * L1 = LISTA CON DOS PUNTOS
 * Busca las posiciones de los puntos en el triangulo,
 * y calcula la distancia entre ellos, RX es la distancia en X
 * y RY es la distancia en Y.
 * EJEMPLO :calcularDistanciaDosPuntos([1,2],R1,R2) --> R1 = 1 , R2=0
 * pues la posicion de 1 en el triangulo es (0,0) y de 2 es (1,0)
 * y la resta, con valor absoluto,entre ellos es (1,0)
 * */
calcularDistanciaDosPuntos(L1,RX,RY):- posicionesElems(L1,S1), getElem(0,S1,S2),
    getElem(1,S1,S3), getElem(0,S2,S4),getElem(1,S2,S5),
    getElem(0,S3,S6),getElem(1,S3,S7),restaAbs(S4,S6,RX),restaAbs(S5,S7,RY).


/*RETORNA EL ELEMENTO QUE ESTA EN LA POSICION X DE UNA LISTA DADA
 * EJEMPLO: getElem(1,[1,2,3],S) --> S = 2
 *  */

getElem(0,[C|_],C):-!. 

getElem(X,[_|R],Sol):- X1 is X -1, getElem(X1,R,Sol). 

/*CALCULA LA RESTA DE DOS NÚMEROS, CON VALOR ABSOLUTO
 * EJEMPLO: restaAbs(2,3,R)--> R= 1*/
restaAbs(N,K,R):- R is abs(N-K).

/*RETORNA LAS POSICIONES DE LOS ELEMENTOS, DADOS EN UNA LISTA, 
 * DENTRO DEL TRIANGULO
 * EJEMPLO: posicionesElems([1,2,3],S).
 * S = [[1, 1], [1, 0], [0, 0]]
 * */

posicionesElems([],[]).
posicionesElems([X|Xs], S):- posicionesElems(Xs, S2),triangulo(90,4095,Triangulo),
          buscarPosElem(Triangulo,X,R1), insFinal(S2,R1,S).


/*Genera el triangulo pascal sobre el cuál estan los puntos de las figuras
 * (el universo sobre el que se pueden formar)
 * N es la cantidad de niveles del triangulo
 * M es el ultimo elemento de la lista
 * EJEMPLO: triangulo(6,21,L)--> 
 * L= [[1], [2, 3], [4, 5, 6], [7, 8, 9, 10], [11, 12, 13, 14, 15], [16, 17, 18, 19, 20, 21]]*/
triangulo(0,_,[]):-!.
triangulo(N,M,S):- T2 is (M+1)-N, N1 is N-1, T3 is M+1,
    genera(T2,T3,S2), T4 is M-N,
    triangulo(N1,T4,S3),
    insFinal(S3,S2,S),!.
    
/*
 * Retorna una lista con elementos de M a N-1
 * EJEMPLO: general(3,10,L)--> L= [3,4,5,6,7,8,9]*/    
genera(M,M,[]).
genera(M,N,[M|L]):- M < N, M1 is M+1, genera(M1,N,L).

/*INSERTA ELEMENTOS AL FINAL DE UNA LISTA
 * insFinal([1,2,3,4],5, NuevaLista). ---- NuevaLista = [1, 2, 3, 4, 5]
 * 
 * */
insFinal([], E, [E]).
insFinal([Cabeza|Resto], Elemento, [Cabeza|Lista]):-
        insFinal(Resto, Elemento, Lista). 

/* Predicado auxiliar "indexOf" para obtener 
* la posicion de un
* elemento en una lista
* */
indexOf([Element|_],Element,0):-!.
indexOf([_|Tail],Element,Index):-
	indexOf(Tail,Element,Index1),
	Index is Index1 + 1.

/*Predicado auxiliar "buscarPorFila" 
* para buscar un elemento
* en una lista de listas*/

buscarPorFila([],_,_,[0,0]).
buscarPorFila([Head|Tail],Element,Fila,[X,Y]):-
			   (member(Element,Head)->
			   		indexOf(Head,Element,N1),
			   		Y is N1,
			   		X is Fila,
			   		!
			   	; 
			   	  Fila1 is Fila + 1,
			   	  buscarPorFila(Tail,Element,Fila1,[X,Y])
			   	).

/* RETORNA LAS POSICIONES X,Y DE UN ELEMENTO EN UNA LISTA DE LISTAS
 * 
 * buscarPorFila([[1],[2,3],[4,5,6],[7,8,9,10]],9,0,R)
 * R = [3, 2]
 * 
 * */

buscarPosElem([Head|Tail],Element,[X,Y]):-
			   buscarPorFila([Head|Tail],Element,0,[X1,Y1]),
			   X is X1,
			   Y is Y1.
/*
 * Elimina las repeticiones de elementos de una lista.
 * EJEMPLOS: filtra([0,0,0,2,3,1,1,3,2],R) --> R = [0, 2, 3, 1]
 *           filtra([0],R) --> R= [0]
 *           filtra([1,2,3,4,5,5,5],R) --> R = [1, 2, 3, 4, 5]
 * */
filtra([ X | Xs ], [ X | Ys ] ) :- borra( X, [ X | Xs ], Zs ), filtra(Zs,Ys),!.
filtra([], []).

/*
 * borra( X, Xs, Ys ) : Borra todas las apariciones de X de la lista Xs 
 * y obtiene una nueva lista YS .
 * EJEMPLO: borra(0,[1,9,0,3,0],R)--> R = [1,9,3]*/

borra(X, [X |Xs],Zs):- borra(X,Xs,Zs) .
borra(X, [Y |Xs], [Y |Zs]):- X \== Y,borra(X, Xs,Zs).  
borra( _,[],[]).

/*
 * Valida si un elemento X pertenece a una lista dada.
 * ejemplo: existe(1,[1,2,3])--> true
 *          existe(10,[1,2,3])--> false
 * */
existe(_,[]):-fail. 
existe(X,[X|_]):-!. 
existe(X,[_|R]):-existe(X,R).

/*
 * DADA UNA LISTA RETORNA SU LONGITUD 
 * */
largoLista([ _| Xs ], I, M ) :- I1 is I + 1, largoLista( Xs, I1, M ) .
largoLista([], M, M ).
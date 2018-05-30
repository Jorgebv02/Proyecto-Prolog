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

my_sum_elements([],[]).

my_sum_elements([X|Xs], S):-
          my_sum_elements(Xs, S2),
          calcularDistanciaDosPuntos(X,S3,S4),
          insFinal(S2,S3,S5), insFinal(S5,S4,S).

long( [ _| Xs ], I, M ) :- I1 is I + 1, long( Xs, I1, M ) .
long( [], M, M ).

proc22(L1,L2):- combinatoriaParalelogramo(L1,R), my_sum_elements(R,L2).

filtra( [ X | Xs ], [ X | Ys ] ) :- borra( X, [ X | Xs ], Zs ), filtra( Zs, Ys ) .
filtra( [], [] ) .

% borra( X, Xs, Ys ) : Borra el elemento X de la lista Xs y obtiene una 
% nueva lista YS .
borra( X, [ X | Xs ], Zs ) :- borra( X, Xs, Zs ) .
borra( X, [ Y | Xs ], [ Y | Zs ] ) :- X \== Y, borra( X, Xs, Zs ) .  
borra( _, [], [] ) .

existe(_,[]):-fail. 
existe(X,[X|_]):-!. 
existe(X,[_|R]):- 
existe(X,R).

esParalelogramo(L):- proc22(L,R1),filtra(R1,R2),long(R2,0,M),
    M=2, existe(0,R2),!.  
    
/*
 * L1 = LISTA CON DOS PUNTOS
 * EJEMPLO : [1,2]   [1,4] 
 * */

calcularDistanciaDosPuntos(L1,R1,R2):- proc(L1,S1), getElem(0,S1,S2),
    getElem(1,S1,S3), getElem(0,S2,S4),getElem(1,S2,S5),
    getElem(0,S3,S6),getElem(1,S3,S7),restaAbs(S4,S6,R1),restaAbs(S5,S7,R2).
    
restaAbs(N,K,R):- R is abs(N-K).

/*RETORNA EL ELEMENTO EN LA POSICION X DE UNA LISTA*/
getElem(0,[C|_],C):-!. 
getElem(X,[_|R],Sol):- 
X1 is X -1, 
getElem(X1,R,Sol). 


/*RETORNA LAS POSICIONES DE LOS ELEMENTOS DADOS DENTRO DEL TRIANGULO
 * proc([1,2,3],S).
 * S = [[1, 1], [1, 0], [0, 0]]
 * */
proc([],[]).
proc([X|Xs], S):-
          proc(Xs, S2),buscarPosElem([[1],[2,3],[4,5,6],[7,8,9,10],[11,12,13,14,15],[16,17,18,19,20,21],[22,23,24,25,26,27,28]],X,R1),
    insFinal(S2,R1,S).


/*INSERTA ELEMENTOS AL FINAL DE UNA LISTA
 * nsFinal([1,2,3,4],5, NuevaLista). ---- NuevaLista = [1, 2, 3, 4, 5]
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
 * buscarPorFila([[1],[2,3],[4,5,6],[7,8,9,10]],9,0,R)
 * R = [3, 2]
 * 
 * */
buscarPosElem([Head|Tail],Element,[X,Y]):-
			   buscarPorFila([Head|Tail],Element,0,[X1,Y1]),
			   X is X1,
			   Y is Y1.
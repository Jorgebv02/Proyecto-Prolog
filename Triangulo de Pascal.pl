/*
**************************************************
Código fuente: proyecto.pl
Versión: 1.0
Fecha de creación: 28 de mayo del 2018.
Última actualización: N/A.
Autores: Jorge Barquero Villalobos,
         Juan Diego Escobar Sánchez,
         Yericka Lafuente Ovares,
         basados en la Prof. Ericka Solano Fernández.
Descripción: programa que, dado un archivo con posiciones 
en un triángulo de Pascal, retorna si es una figura geométrica.
Notas: utilizando Learn Prolog Now!

Construido con: SWI-Prolog 7.6.4.
**************************************************
*/

% Función auxiliar para la construcción de cada lista 
% con pesos del triángulo.
% Cuando ya solo le quede de calcular dos listas, es Cuando
% se detiene.
pascalFila([X], [X]).	

% En caso contrario, divide la lista en dos, por así decirlo.
% Luego, simplemente toma el último parámetro que esté antes
% de el, y lo suma con el de la par para así poder hacer la 
% recreación del árbol:
% 			1
%		1		1
%	1		2		1
pascalFila([H, H2|T], [A|B]):-
    pascalFila([H2|T], B),
    % Entonces, el siguiente es la suma de los dos anteriores.
    A is H + H2.

% Función principal que construye el triángulo de pascal. 
% Básicamente, no sé exactamente por que me retorna el árbol 
% como en reversa, es decir, me devuelve algo así: 
% [[1, 3, 3, 1], [1, 2, 1], [1, 1], [1]], seguro es por la definición, 
% pero devuelve la primera fila del árbol como el último elemento. 
pascal(0, [[1]]) :- !.

% Aquí solo debe de decrementar el contador de por cuál ve el 
% nivel de profundidad. 
pascal(N, [R, R1 | RN]) :-
    N1 is N-1,
    pascal(N1, [R1 | RN]),
    % El 0 es como para poder hacer la separación de filas. 
    pascalFila([0|R1], R).
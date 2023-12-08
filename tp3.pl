
% ejemplo(+Codigo, -E)
ejemplo(c4, [(n1,n2),(n2,n3),(n3,n4),(n4,n1)]).
ejemplo(c6, [(n1,n2),(n3,n4),(n5,n6)]).
ejemplo(c3, [(n1,n2),(n1,n3),(n2,n3)]).


% Ejercicio 1
% armar_grafo(+E,-Grafo).
% iteramos sobre las aristas, agregando los nodos que no estaban
% previamente
agregar_nodo(N, Cs, Cs) :- member((N, _), Cs).
agregar_nodo(N, Cs, Cs1) :- not(member((N, _), Cs)), Cs1 = [(N, _)|Cs].
armar_grafo([], grafo([],[])).
armar_grafo([(N1,N2)|E], grafo(Cs,L)) :- armar_grafo(E, grafo(Cs1, L1)), L = [(N1,N2)|L1], agregar_nodo(N1, Cs1, Cs2), agregar_nodo(N2, Cs2, Cs).


% Ejercicio 2
% color_nodo(+Grafo, +Nodo, ?Color).
color_nodo(grafo(Cs, _), N, C) :- member((N, C), Cs).


% Ejercicio 3
% vecino(+G, ?V, ?W).
vecino(grafo(_, L), V, W) :- member((V, W), L).
vecino(grafo(_, L), V, W) :- member((W, V), L).


% Ejercicio 4
% colores_vecinos(+G, +Nodo, -Colores).
% buscamos todos los vecinos y luego sus colores, de estar ya definidos
agregar_colores(_, [], []).
agregar_colores(grafo(Cs, _), [W|Ws], Col) :- color_nodo(grafo(Cs, _), W, C), nonvar(C), agregar_colores(grafo(Cs, _), Ws, Col1), Col = [C|Col1].
agregar_colores(grafo(Cs, _), [W|Ws], Col) :- color_nodo(grafo(Cs, _), W, C), var(C), agregar_colores(grafo(Cs, _), Ws, Col).
colores_vecinos(grafo(Cs, L), N, Col) :- findall(W, vecino(grafo(Cs, L), N, W), Ws), agregar_colores(grafo(Cs, L), Ws, Col).


% Ejercicio 5
% pintar_nodo(+Paleta, ?Grafo, +Nodo).
% buscamos los colores de los vecinos y pintamos con uno de la paleta
% que no esté entre esos
% Grafo puede o no tener definido color para Nodo
pintar_nodo(P, grafo(Cs, L), N) :- colores_vecinos(grafo(Cs, L), N, Col), between(1, P, C), not(member(C, Col)), color_nodo(grafo(Cs, L), N, C).


% Ejercicio 6
% pintar_grafo(+Paleta, ?Grafo).
% buscamos todos los nodos y pintamos cada uno de ellos
% Grafo puede o no tener definidos colores para cada nodo
nodos_grafo(grafo([], _), []).
nodos_grafo(grafo([(N, _)|Cs], _), Ns) :- nodos_grafo(grafo(Cs,_), Ns1), Ns = [N|Ns1].
pintar_nodos(_, _, []).
pintar_nodos(P, G, [N|Ns]) :- pintar_nodo(P, G, N), pintar_nodos(P, G, Ns).
pintar_grafo(P, G) :- nodos_grafo(G, Ns), pintar_nodos(P, G, Ns).


% Ejercicio 7
% mismo_color(+G,+V,+W)
% si no tenian color, reemplazamos la variable que representa al color
% de W con la misma que a V, sino compara que sean el mismo valor
mismo_color(grafo(Cs, _), V, W) :- member((V, X), Cs), member((W, X), Cs).


% Ejercicio 8
% es_valido(+Grafo)
% por cada arista del grafo vemos si los colores de los nodos que
% conecta son distintos
es_valido(grafo(_, [])).
es_valido(grafo(Cs, [(N1,N2)|L])) :- not(mismo_color(grafo(Cs, _), N1, N2)), es_valido(grafo(Cs, L)).


% Ejercicio 9
% coloreo(+G, -Coloreo).
% pintamos el grafo y buscamos el color mas alto que recibio algun nodo
% luego nos aseguramos que todos los colores intermedios aparezcan al
% menos una vez para no tener huecos
color_max([], 0).
color_max([(_,C)|Cs], M) :- color_max(Cs, M1), M is max(M1, C).
coloreo(grafo(Cs, L), Cs) :- length(Cs, P), pintar_grafo(P, grafo(Cs, L)), color_max(Cs, M), forall(between(1, M, C), member((_, C), Cs)).

%TESTS
test(1) :- ejemplo(c4, E), armar_grafo(E, G), color_nodo(G, n4, 3), colores_vecinos(G, n1, [3]).
test(2) :- ejemplo(c4, E), armar_grafo(E, G), color_nodo(G, n4, 3), color_nodo(G,n2,2), colores_vecinos(G, n1, LC), sort(LC,[2,3]).
test(3) :- ejemplo(c4, E), armar_grafo(E, G), colores_vecinos(G, n1, []).
test(4) :- es_valido(grafo([(n2, 1),  (n3, 2),  (n4, 1),  (n1, 2)], [(n1, n2),  (n2, n3),  (n3, n4),  (n4, n1)])).
test(5) :- not(es_valido(grafo([(n2, 1),  (n3, 2),  (n4, 1),  (n1, 1)], [(n1, n2),  (n2, n3),  (n3, n4),  (n4, n1)]))).
test(6) :- findall(CS,(ejemplo(c4, E), armar_grafo(E, G), coloreo(G, CS)),L), length(L,38).
test(7) :- ejemplo(c4, E), armar_grafo(E, G), coloreo(G, CS), sort(CS, [(n1, 2), (n2, 1), (n3, 2), (n4, 3)]).


% test 8: grafo simple
test(8) :- ejemplo(c3, E), armar_grafo(E, G), G = grafo(Cs, L), length(Cs, 3), length(L, 3), E = L, member((n1,X),Cs), member((n2,Y),Cs), member((n3,Z),Cs), var(X), var(Y), var(Z).
% test 9: asignar color
test(9) :- ejemplo(c6, E), armar_grafo(E, G), color_nodo(G, n2, 4), G = grafo(Cs, _), length(Cs, 6), member((n2,4), Cs).
% test 10: C puede no estar instanciado
test(10) :- ejemplo(c6, E), armar_grafo(E, G), color_nodo(G, n2, 4), color_nodo(G, n2, C), C is 4.
% test 11: buscar vecinos
test(11) :- ejemplo(c3, E), armar_grafo(E, G), findall(W, vecino(G, n1, W), Ws), length(Ws, 2), member(n2, Ws), member(n3, Ws).
% test 12: colores de vecinos sin asignar no aparecen
test(12) :- ejemplo(c3, E), armar_grafo(E, G), colores_vecinos(G, n1, C), C = [].
% test 13: colores de vecinos asignados aparecen
test(13) :- ejemplo(c3, E), armar_grafo(E, G), color_nodo(G, n1, 3), color_nodo(G, n2, 2), color_nodo(G, n3, 4), colores_vecinos(G, n2, C), length(C, 2), member(3, C), member(4, C).
% test 14: pintar nodo
test(14) :- ejemplo(c6, E), armar_grafo(E, G), G = grafo(Cs, L), findall(Cs, pintar_nodo(3, grafo(Cs, L), n2), Css), flatten(Css, Css1), member((n2, 1), Css1), member((n2, 2), Css1), member((n2, 3), Css1).
% test 15: 5 * 4 * 3 combinaciones de colores
test(15) :- ejemplo(c3, E), armar_grafo(E, G), findall(G, pintar_grafo(5, G), Gs), length(Gs, 60).
% test 16: 2 * 2 * 2 combinaciones de colores
test(16) :- ejemplo(c6, E), armar_grafo(E, G), findall(G, pintar_grafo(2, G), Gs), length(Gs, 8).
% test 17: no pueden tener el mismo color al estar conectados
test(17) :- ejemplo(c3, E), armar_grafo(E, G), mismo_color(G, n1, n2), findall(G, pintar_grafo(3, G), Gs), Gs = [].
% test 18: 2 combinaciones ya que los pares de nodos comienzan con el
% mismo color
test(18) :- ejemplo(c6, E), armar_grafo(E, G), mismo_color(G, n1, n3), mismo_color(G, n3, n5), findall(G, pintar_grafo(2, G), Gs), length(Gs, 2).
% test 19: grafo no valido
test(19) :- ejemplo(c3, E), armar_grafo(E, G), color_nodo(G, n1, 2), color_nodo(G, n2, 2), color_nodo(G, n3, 1), not(es_valido(G)).
% test 20: grafo valido
test(20) :- ejemplo(c3, E), armar_grafo(E, G), color_nodo(G, n1, 1), color_nodo(G, n2, 2), color_nodo(G, n3, 3), es_valido(G).
% test 21: coloreo grafo conexo
test(21) :- ejemplo(c3, E), armar_grafo(E, G), findall(Col, coloreo(G, Col), Cols), length(Cols, 6), forall(member(Col, Cols), (member((_, 1), Col), member((_, 2), Col), member((_, 3), Col), length(Col, 3))).
% test 22: coloreo grafo de a pares, fijando color de n1 hay 2
% posibilidades para el resto del grafo y puede haber hasta 3 colores
% maximo sin saltear colores, por lo que hay 8 coloreos distintos.
test(22) :- ejemplo(c6, E), armar_grafo(E, G), mismo_color(G, n1, n3), mismo_color(G, n3, n5), mismo_color(G, n2, n4), findall(Col, coloreo(G, Col), Cols), length(Cols, 8).


tests :- forall(between(1,22,N), test(N)). % Hacer sus propios tests y cambiar el 10 por la cantidad de tests que tengan.

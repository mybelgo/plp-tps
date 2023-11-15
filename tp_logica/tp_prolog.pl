
% ejemplo(+Codigo, -E)
ejemplo(l2, [(n1, n2)]).
ejemplo(t3, [(n1, n2), (n2, n3), (n3, n1)]).
ejemplo(c4, [(n1, n2), (n2, n3), (n3, n4), (n4, n1)]).

%-----------------------------------------------------------------------------------------------------------------------
% Ejercicio 1
% armar_grafo(+E,-Grafo).
armar_grafo([], grafo([], [])).
armar_grafo(LA, grafo(LNC, LA)) :-
  % Esta solución hace 2 pasadas, que no es optimo pero es la que usa implementaciones
  % mas generales de predicados auxiliares.
  nodos(LA, LN),
  nodos_coloreados(LN, LNC).

% nodos_coloreados(+LN, -LNC).
% Dada una lista de nodos, devuelve una lista de pares de nodos y colores.
nodos_coloreados([], []).
nodos_coloreados([N | LN], [(N, _) | LNC]) :-
  nodos_coloreados(LN, LNC).

% nodos(+LA, -LN).
% Dada una lista de aristas, devuelve la lista de sus nodos.
nodos([], []).
nodos([(X1, X2) | XS], N) :-
  nodos(XS, RN),
  agregar(X1, RN, RN1),
  agregar(X2, RN1, N).

% agregar(+X, +L, -R).
% Agrega un elemento a la lista, si no está ya.
agregar(X, L, L) :- member(X, L).
agregar(X, L, [X | L]) :- not(member(X, L)).

%-----------------------------------------------------------------------------------------------------------------------
% Ejercicio 2
% color_nodo(+Grafo, +Nodo, ?Color).
color_nodo(grafo(LNC, _), N, C) :-
  member((N, C), LNC).

%-----------------------------------------------------------------------------------------------------------------------
% Ejercicio 3
% vecino(+G, ?V, ?W).
% Esto no da resultados duplicados porque no valen elementos simétricos.
vecino(grafo(_, LA), V, W) :- member((V, W), LA).
vecino(grafo(_, LA), V, W) :- member((W, V), LA).

%-----------------------------------------------------------------------------------------------------------------------
% Ejercicio 4
% colores_vecinos(+G, +Nodo, -Colores).
colores_vecinos(grafo([], _), _, []).
colores_vecinos(G, X, [C | LC]) :-
  G = grafo([(N, C) | LNC], LA),
  vecino(G, N, X),
  nonvar(C),
  colores_vecinos(grafo(LNC, LA), X, LC).
colores_vecinos(G, X, LC) :-
  G = grafo([(N, C) | LNC], LA),
  not(vecino(G, N, X)),
  nonvar(C),
  colores_vecinos(grafo(LNC, LA), X, LC).
colores_vecinos(G, X, LC) :-
  G = grafo([(_, C) | LNC], LA),
  var(C),
  colores_vecinos(grafo(LNC, LA), X, LC).

%-----------------------------------------------------------------------------------------------------------------------
% Ejercicio 5
% pintar_nodo(+Paleta, ?Grafo, +Nodo).
pintar_nodo(P, G, N) :-
  between(1, P, C),
  colores_vecinos(G, N, LC),
  not(member(C, LC)),
  color_nodo(G, N, C).

%-----------------------------------------------------------------------------------------------------------------------
% Ejercicio 6
% pintar_grafo(+Paleta, ?Grafo).
pintar_grafo(P, grafo(LNC, LA)) :-
  nodos(LA, LN),
  maplist(pintar_nodo(P, grafo(LNC, LA)), LN).

%-----------------------------------------------------------------------------------------------------------------------
% Ejercicio 7
% mismo_color(+G,+V,+W)
mismo_color(G, V, W) :-
  color_nodo(G, V, C),
  color_nodo(G, W, C).

%-----------------------------------------------------------------------------------------------------------------------
% Ejercicio 8
% es_valido(+Grafo)
es_valido(grafo([], _)).
es_valido(G) :-
  G = grafo([(N, C) | LNC], LA),
  es_valido(grafo(LNC, LA)),
  colores_vecinos(G, N, LCV),
  not(member(C, LCV)).

%-----------------------------------------------------------------------------------------------------------------------
% Ejercicio 9
% coloreo(+G, -Coloreo).
coloreo(grafo(LNC, LA), LNC) :-
  length(LNC, L),
  pintar_grafo(L, grafo(LNC, LA)),
  sin_huecos(LNC).

% sin_huecos(+P, +LNC).
% Verifica si la lista de nodos coloreados no incluye huecos.
sin_huecos(LNC) :-
  max_de_los_colores(LNC, CM),
  forall(between(1, CM, C), member((_, C), LNC)).

% max_de_los_colores(+LNC).
% Devuelve el máximo color usado en el coloreo.
max_de_los_colores([(_, C)], C).
max_de_los_colores([(_, C) | LNC], C) :- max_de_los_colores(LNC, RC), C >= RC.
max_de_los_colores([(_, C) | LNC], RC) :- max_de_los_colores(LNC, RC), C < RC.

%-----------------------------------------------------------------------------------------------------------------------
%TESTS
test(1) :- ejemplo(c4, E), armar_grafo(E, G), color_nodo(G, n4, 3), colores_vecinos(G, n1, [3]).
test(2) :- ejemplo(c4, E), armar_grafo(E, G), color_nodo(G, n4, 3), color_nodo(G, n2, 2), colores_vecinos(G, n1, LC), sort(LC, [2, 3]).
test(3) :- ejemplo(c4, E), armar_grafo(E, G), colores_vecinos(G, n1, []).
test(4) :- es_valido(grafo([(n2, 1),  (n3, 2),  (n4, 1),  (n1, 2)], [(n1, n2),  (n2, n3),  (n3, n4),  (n4, n1)])).
test(5) :- not(es_valido(grafo([(n2, 1),  (n3, 2),  (n4, 1),  (n1, 1)], [(n1, n2),  (n2, n3),  (n3, n4),  (n4, n1)]))).
test(6) :- findall(CS, (ejemplo(c4, E), armar_grafo(E, G), coloreo(G, CS)), L), length(L, 38).
test(7) :- ejemplo(c4, E), armar_grafo(E, G), coloreo(G, CS), sort(CS, [(n1, 2), (n2, 1), (n3, 2), (n4, 3)]).
test(8) :- ejemplo(t3, E), armar_grafo(E, G), coloreo(G, CS), sort(CS, [(n1, 1), (n2, 2), (n3, 3)]).
test(9) :- ejemplo(l2, E), armar_grafo(E, G), coloreo(G, CS), sort(CS, [(n1, 2), (n2, 1)]).

tests :- forall(between(1, 9, N), test(N)).

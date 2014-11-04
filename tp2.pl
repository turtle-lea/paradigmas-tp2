%Autómatas de ejemplo. Si agregan otros,  mejor.

ejemplo(1, a(s1, [sf], [(s1, a, sf)])).
ejemplo(2, a(si, [si], [(si, a, si)])).
ejemplo(3, a(si, [si], [])).
ejemplo(4, a(s1, [s2, s3], [(s1, a, s1), (s1, a, s2), (s1, b, s3)])).
ejemplo(5, a(s1, [s2, s3], [(s1, a, s1), (s1, b, s2), (s1, c, s3), (s2, c, s3)])).
ejemplo(6, a(s1, [s3], [(s1, b, s2), (s3, n, s2), (s2, a, s3)])).
ejemplo(7, a(s1, [s2], [(s1, a, s3), (s3, a, s3), (s3, b, s2), (s2, b, s2)])).
ejemplo(8, a(s1, [sf], [(s1, a, s2), (s2, a, s3), (s2, b, s3), (s3, a, s1), (s3, b, s2), (s3, b, s4), (s4, f, sf)])). % No deterministico :)
ejemplo(9, a(s1, [s1], [(s1, a, s2), (s2, b, s1)])).
ejemplo(10, a(s1, [s10, s11],
        [(s2, a, s3),(s4, a, s5), (s9, a, s10), (s5, d, s6), (s7, g, s8), (s15, g, s11), (s6, i, s7), (s13, l, s14), (s8, m, s9), (s12, o, s13), (s14, o, s15), (s1, p, s2), (s3, r, s4), (s2, r, s12), (s10, s, s11)])).

ejemploMalo(1, a(s1, [s2], [(s1, a, s1), (s1, b, s2), (s2, b, s2), (s2, a, s3)])). %s3 es un estado sin salida.
ejemploMalo(2, a(s1, [sf], [(s1, a, s1), (sf, b, sf)])). %sf no es alcanzable.
ejemploMalo(3, a(s1, [s2, s3], [(s1, a, s3), (s1, b, s3)])). %s2 no es alcanzable.
ejemploMalo(4, a(s1, [s3], [(s1, a, s3), (s2, b, s3)])). %s2 no es alcanzable.
ejemploMalo(5, a(s1, [s3, s2, s3], [(s1, a, s2), (s2, b, s3)])). %Tiene un estado final repetido.
ejemploMalo(6, a(s1, [s3], [(s1, a, s2), (s2, b, s3), (s1, a, s2)])). %Tiene una transición repetida.
ejemploMalo(7, a(s1, [], [(s1, a, s2), (s2, b, s3)])). %No tiene estados finales.

%%%%%%% ejemplos propios

%%%%%%%

%%Proyectores
inicialDe(a(I, _, _), I).

finalesDe(a(_, F, _), F).

transicionesDe(a(_, _, T), T).

%Auxiliar dada en clase
%desde(+X, -Y).
desde(X, X).
desde(X, Y):-desde(X, Z),  Y is Z + 1.


%%Predicados pedidos.

% 1) %esDeterministico(+Automata)

esDeterministico(A) :- transicionesDe(A, T), not(transicionSimilar(T)).
transicionSimilar(T) :- member((Origen,Etiqueta,X),T), member((Origen,Etiqueta,Y),T), X \= Y.

% 2) estados(+Automata, ?Estados)
estados(A, Estados) :- nonvar(Estados), not((member(E,Estados),esEstadoInvalido(E,A))), not(faltaUno(A,Estados)).
estados(a(I, Finales, T), Estados) :- 	var(Estados),
										E1 = [I | Finales],
										estadosDeTransiciones(T, E2),
										append(E1, E2, E3),
										sort(E3, Estados).

faltaUno(A, Estados) :- transicionesDe(A,T), member((Origen,_,_),T), not(member(Origen,Estados)).
faltaUno(A, Estados) :- transicionesDe(A,T), member((_,_,Destino),T), not(member(Destino,Estados)).
faltaUno(A, Estados) :- inicialDe(A,X), not(member(X,Estados)).
faltaUno(A, Estados) :- finalesDe(A,F), member(X,F), not(member(X,Estados)).

esEstadoInvalido(E, a(Inicial,Finales,Transiciones)) :- (Inicial \= E), not(member(E,Finales)), noAparece(E,Transiciones).

noAparece(E,Transiciones) :- not(member((E,_,_),Transiciones)), not(member((_,_,E),Transiciones)).

estadosDeTransiciones([], []).
estadosDeTransiciones([(X,_,Y) | XS], Estados) :- estadosDeTransiciones(XS, E), Estados = [X,Y|E].


% 3)esCamino(+Automata, ?EstadoInicial, ?EstadoFinal, +Camino)
esCamino(A, S1, S2, Camino) :- nth0(0,Camino,S1), last(Camino,S2), caminoValido(A,Camino).

caminoValido(A,[X]) :- estados(A,E), member(X,E).
caminoValido(A,[X,Y|Tail]) :- transicionesDe(A,T), member( (X,_,Y), T), caminoValido(A,[Y|Tail]).

% 4) ¿el predicado anterior es o no reversible con respecto a Camino y ppor que?

% Responder aqui.

% 5) caminoDeLongitud(+Automata, +N, -Camino, -Etiquetas, ?S1, ?S2)

caminoDeLongitud(A, 1, [X], [], X, Y) :- X=Y, estados(A,Es), member(X,Es).
caminoDeLongitud(A, N, Camino, Etiquetas, S1, S2) :-
	NmenosUno is N-1,
	NmenosUno+1 > 1,
	transicionesDe(A,T),
	caminoDeLongitud(A,NmenosUno, RestoCamino, RestoEtiquetas, Svecino, S2),
	member( (S1,E,Svecino), T),
	Camino = [S1 | RestoCamino],
	Etiquetas = [E | RestoEtiquetas].

% 6) alcanzable(+Automata, +Estado)

%Disclaimer: el estado inicial no es alcanzable desde sí mismo a menos que forme parte de un ciclo, tal como dice el enunciado.

%alcanzable(A, E) :- estados(A,Es),
%										length(Es,Ncota),
%										nAlcanzable(A, E, Ncota).

%nAlcanzable(A, E, Ncota) :- inicialDe(A, S1),
%												between(2, Ncota, N),
%												caminoDeLongitud(A, N, _, _, S1, E), !.

alcanzable(A,E) :- inicialDe(A,S1), alcanzableDesde(A,S1,E).

alcanzableDesde(A,E1,E2) :- estados(A,Es), length(Es,L), Ncota is L+1, nAlcanzableDesde(A,E1,E2,Ncota).

nAlcanzableDesde(A,E1,E2,Ncota) :- between(2,Ncota,N), caminoDeLongitud(A, N, _, _, E1, E2), !. 

% 7) automataValido(+Automata)
automataValido(A) :- tienenTransicionesSalientes(A),
										 sonAlcanzables(A),
										 tieneFinal(A),
										 noHayFinalesRepetidos(A),
										 noHayTransicionesRepetidas(A).

tienenTransicionesSalientes(A) :- estados(A,Es),
																	transicionesDe(A,T),
																	finalesDe(A,F),
                                  forall( (member(E,Es), not(member(E,F))), member((E,_,_),T) ).

%Chequea si todos los estados son alcanzados desde el estado inicial, salvo este último.
sonAlcanzables(A) :- estados(A, Es), inicialDe(A,I),
											forall(
												(member(E, Es), E \= I),
												alcanzable(A, E)
											).

tieneFinal(A) :- finalesDe(A,F), length(F,T), T > 0.

noHayFinalesRepetidos(A) :- finalesDe(A,F), sinRepetidos(F).

noHayTransicionesRepetidas(A) :- transicionesDe(A,T), sinRepetidos(T).

%Aprovechamos que el predicado sort elimina repetidos.
sinRepetidos(Lista) :- length(Lista,L), sort(Lista,ListaOrdenada), length(ListaOrdenada,L2), L =:= L2.


%--- NOTA: De acá en adelante se asume que los autómatas son válidos.


% 8) hayCiclo(+Automata)
%hayCiclo(A):- estados(A,Estados), length(Estados, CantEstados), Cota is CantEstados + 1,
%             caminoDeLongitud(A, Cota, _ , _, _, _), !.

hayCiclo(A) :- estados(A, Es),
  member(E, Es),
  alcanzableDesde(A,E,E),!.

% 9) reconoce(+Automata, ?Palabra)
reconoce(A, P) :- nonvar(P), inicialDe(A,I), length(P,N), palabraLongitudN(I,A,N,P).
reconoce(A,P) :- var(P), hayCiclo(A), desde(0,N), inicialDe(A,I), palabraLongitudN(I,A,N,P).
reconoce(A,P) :- var(P), not(hayCiclo(A)), transicionesDe(A,T), length(T,L), between(0,L,N), inicialDe(A,I), palabraLongitudN(I,A,N,P).


palabraLongitudN(E,A,0,[]) :- finalesDe(A,F), member(E,F).
palabraLongitudN(E,A,N,[X|Xs]) :- N>0, transicionesDe(A,T), NmenosUno is N-1, member((E,X,Destino),T), palabraLongitudN(Destino,A,NmenosUno,Xs).

% 10) PalabraMásCorta(+Automata, ?Palabra)
palabraMasCorta(A, P) :- ejemploPalabraMasCorta(A,X), length(X,N), inicialDe(A,I), palabraLongitudN(I,A,N,P).

ejemploPalabraMasCorta(A,P) :- desde(0,N), inicialDe(A,I), palabraLongitudN(I,A,N,P), !.

%-----------------
%----- Tests -----
%-----------------

% Algunos tests de ejemplo. Deben agregar los suyos.

test(1) :- forall(ejemplo(_, A),  automataValido(A)).
test(2) :- not((ejemploMalo(_, A),  automataValido(A))).
test(3) :- ejemplo(10, A), reconoce(A, [p, X, r, X, d, i, _, m, X, s]).
test(4) :- ejemplo(9, A), reconoce(A, [a,  b,  a,  b,  a,  b,  a,  b]).
test(5) :- ejemplo(7, A), reconoce(A, [a,  a,  a,  b,  b]).
test(6) :- ejemplo(7, A), not(reconoce(A, [b])).
test(7) :- ejemplo(2, A),  findall(P, palabraMasCorta(A, P), [[]]).
test(8) :- ejemplo(4, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[a], [b]]).
test(9) :- ejemplo(5, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[b], [c]]).
test(10) :- ejemplo(6, A),  findall(P, palabraMasCorta(A, P), [[b, a]]).
test(11) :- ejemplo(7, A),  findall(P, palabraMasCorta(A, P), [[a, b]]).
test(12) :- ejemplo(8, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[a,  a,  b,  f], [a,  b,  b,  f]]).
test(13) :- ejemplo(10, A),  findall(P, palabraMasCorta(A, P), [[p, r, o, l, o, g]]).
test(14) :- forall(member(X, [2, 4, 5, 6, 7, 8, 9]), (ejemplo(X, A), hayCiclo(A))).
test(15) :- not((member(X, [1, 3, 10]), ejemplo(X, A), hayCiclo(A))).
tests :- forall(between(1, 15, N), test(N)). %IMPORTANTE: Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.


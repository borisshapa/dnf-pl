random(L, R, T) :- Len is R - L, rand_int(Len, W), T is W + L.

inf(T) :- T = 1000000000.

random(T) :- inf(Inf), random(0, Inf, T).

build(T, 0, null, _, T) :- !.

build(T, N, node(X, Y, V, L, R), MN, Suf) :-
    N1 is div(N, 2), N2 is N - N1 - 1,
		inf(Inf),
    random(MN, Inf, Y),
    build(T, N1, L, Y, [(X, V) | S1]),
    build(S1, N2, R, Y, Suf).

tree_build(T, W) :- length(T, N), build(T, N, W, 0, []).

l(node(_, _, _, L, _), L).

r(node(_, _, _, _, R), R).

y(node(_, Y, _, _, _), Y).

k(node(K, _, _, _, _), K).

v(node(_, _, V, _, _), V).

value(node(X, Y, V, _, _), node(X, Y, V, _, _)).

make(node(X, Y, V, L, R), X, Y, V, L, R).

map_split(null, _, null, null, null) :- !.

map_split(T, K, L1, M1, R1) :-
	make(T, K, Y, V, L1, R1), make(M1, K, Y, V, null, null).

map_split(T, K, L1, M1, R1) :-
    make(T, X, _, _, L, R),
    X > K, l(R1, R_L), r(R1, R), value(T, R1), map_split(L, K, L1, M1, R_L).

map_split(T, K, L1, M1, R1) :-
    make(T, X, _, _, L, R),
    X < K, r(L1, L_R), l(L1, L), value(T, L1), map_split(R, K, L_R, M1, R1).

map_merge(T, T, null) :- !.
map_merge(T, null, T) :- !.

map_merge(W, L, R) :-
	y(L, L_Y), y(R, R_Y),	L_Y =< R_Y, l(L, L_L), r(L, L_R),
	value(W, L), l(W, L_L), r(W, R1), map_merge(R1, L_R, R).

map_merge(W, L, R) :-
	y(L, L_Y), y(R, R_Y),	L_Y > R_Y, l(R, R_L), r(R, R_R),
	value(W, R), r(W, R_R), l(W, L1), map_merge(L1, L, R_L).

map_get(T, K, V) :-
	map_split(T, K, L, M, R),
	v(M, V).

map_merge(T, A, B, C) :-
	map_merge(W, A, B),
	map_merge(T, W, C).

map_put(T, K, V, Res) :-
	map_split(T, K, L, _, R),
	random(Y),
	map_merge(Res, L, node(K, Y, V, null, null), R).

map_replace(T, K, V, Res) :-
	map_split(T, K, L, M, R),
	M \= null,
	random(Y),
	map_merge(Res, L, node(K, Y, V, null, null), R),
	!.

map_replace(T, _, _, T).

map_remove(T, K, Res) :-
	map_split(T, K, L, M, R),
	map_merge(Res, L, R).

map_max(T, W) :-
	r(T, null), k(T, W).

map_max(T, W) :-
	r(T, R), map_max(R, W).

map_floorKey(T, K, W) :-
	map_split(T, K, L, M, _),
	map_solve(L, M, W).

map_solve(L, null, W) :- map_max(L, W), !.

map_solve(_, M, W) :- k(M, W).

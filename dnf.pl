f(and(0, _), 0) :- !.
f(and(_, 0), 0) :- !.
f(and(1, X), X) :- !.
f(and(X, 1), X) :- !.
f(and(X, Y), and(T, Y)) :- f(X, T), !.
f(and(X, Y), and(X, T)) :- f(Y, T), !.

f(or(0, X), X) :- !.
f(or(X, 0), X) :- !.
f(or(1, X), 1) :- !.
f(or(X, 1), 1) :- !.
f(or(X, Y), or(T, Y)) :- f(X, T), !.
f(or(X, Y), or(X, T)) :- f(Y, T), !.

f(not(0), 1) :- !.
f(not(1), 0) :- !.
f(not(or(A, B)), and(not(A), not(B))) :- !.
f(not(and(A, B)), or(not(A), not(B))) :- !.
f(not(not(X)), X) :- !.
f(not(X), not(T)) :- f(X, T), !.

f(and(or(A, B), X), or(and(A, X), and(B, X))) :- !.
f(and(X, or(A, B)), or(and(A, X), and(B, X))) :- !.

%making or(and), also removing 0, 1 if possible

solve(X, Res) :- f(X, T), solve(T, Res), !.
solve(X, X).

%removing equal variables in and

g(or(X, Y), or(T, Y)) :- g(X, T), !.
g(or(X, Y), or(X, T)) :- g(Y, T), !.

g(and(and(A, B), X), and(A, and(B, X))) :- !.

is_not(not(_)).

is_and(and(_, _)).

g(and(not(A), B), and(B, not(A))) :- \+ is_not(B), \+ is_and(B), !.
g(and(not(A), and(B, C)), and(B, and(not(A), C))) :- \+ is_not(B), !.

g(and(A, B), and(A, T)) :- g(B, T), !.

go(W, W, 1) :- !.
go(not(W), W, 0) :- !.
go(and(A, B), W, and(T, B)) :- go(A, W, T), !.
go(and(A, B), W, and(A, T)) :- go(B, W, T), !.

g(and(A, B), and(A, T)) :- go(B, A, T), !.

g(and(0, _), 0) :- !.
g(and(_, 0), 0) :- !.
g(and(1, X), X) :- !.
g(and(X, 1), X) :- !.
g(or(0, X), X) :- !.
g(or(X, 0), X) :- !.

norm(X, Res) :- g(X, T), norm(T, Res), !.
norm(X, X).

dnf(X, Res) :- solve(X, T), norm(T, Res).
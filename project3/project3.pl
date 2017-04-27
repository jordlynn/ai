isA(X, Y, parentOf) :- father(X, Y); mother(X, Y).

isA(X, Y, childOf) :- father(X, Y); mother(X, Y).

isA(X, Z, grandparent) :- isA(X, Y, parentOf), isA(Y, Z, parentOf).

isA(X, Y, siblings) :- isA(Z, X, parentOf), isA(Z, Y, parentOf).

isA(X, Y, cousins) :- isA(M, X, parentOf), isA(N, Y, parentOf), isA(M, N, siblings).

/* ===== Recursive functions ===== */

isA(X, Z, ancestor) :- isA(X, Z, parentOf).
isA(X, Z, ancestor) :- isA(Y, X, parentOf), isA(Y, Z, ancestor).

isA(X, Z, decendent) :- isA(X, Z, childOf).
isA(X, Z, decendent) :- isA(Y, X, childOf), isA(Y, Z, decendent).

/* This one was surpisingly easy, more checking maybe? */
isA(X, Y, related) :- isA(X, Y, siblings); isA(X,Y,cousins); X == Y.
isA(X, Y, related) :- isA(Z, X, parentOf), isA(Z, Y, related).

isA(X, Y, nthCuz) :- isA(Z, X, ancestor), isA(Z, Y, ancestor).

father(mark, jordan).
father(mark, kenzie).
father(jeff, mark).
father(jim, steph).
father(jim, mike).
father(jim, janae).
father(mike, kimmy).
father(wayne, colton).
father(james, cyp).
father(cyp, jeff).


mother(janae, colton).
mother(marge, janae).
mother(marge, mike).
mother(marge, steph).
mother(steph, jordan).
mother(steph, kenzie).
mother(carolyn, mark).


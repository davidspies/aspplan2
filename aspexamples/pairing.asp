{pair(C,D)} :- cell(C); cell(D); C < D.
:- {pair(C,Y) : cell(Y); pair(X,C) : cell(X)} > 1; cell(C).

choose(C) | choose(D) :- pair(C,D).
connected(start).
choose(end).
connected(C) :- adjacent(C,D); connected(D); choose(C).
choose(X) :- cell(X); connected(end).

:- not connected(end).

#show pair/2.

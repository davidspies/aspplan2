#const nlayers=3.
#const ninputs=4.
#const bound=-1.

layer(1..nlayers).
input(1..ninputs).

gate(g(L,R)) :- input(L); input(R); L < R.

{lefted(L,N):input(L)} :- layer(N).
{gate_at(g(L,R),N):gate(g(L,R)),not lefted(R,N)} = 1 :- gate(G); lefted(L,N).
righted(R,N) :- gate_at(g(_,R),N); layer(N); input(R).
:- {gate_at(g(_,R),N)} > 1; layer(N); input(R).

:- {gate_at(_,_)} > bound; bound >= 0.
:~ gate_at(G,N); bound=-2.[1,G,N]

value(X,0,0) | value(X,1,0) :- input(X).
value(X,V,N) :- layer(N); value(X,V,N-1); not lefted(X,N); not righted(X,N).
value(L,0,N) :- layer(N); value(L,0,N-1); lefted(L,N).
value(R,1,N) :- layer(N); value(R,1,N-1); righted(R,N).
value(L,V,N) :- layer(N); value(L,1,N-1); value(R,V,N-1); gate_at(g(L,R),N).
value(R,V,N) :- layer(N); value(R,0,N-1); value(L,V,N-1); gate_at(g(L,R),N).
ordered(X,X+1) :- input(X); input(X+1); value(X,0,nlayers).
ordered(X,X+1) :- input(X); input(X+1); value(X+1,1,nlayers).
sorted :- ordered(X,X+1) : input(X),input(X+1).
value(X,0,0) :- input(X); sorted.
value(X,1,0) :- input(X); sorted.
:- not sorted.

#show gate_at/2.

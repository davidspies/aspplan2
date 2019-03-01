action(_preserve(F)) :- fluent(F).
pre(_preserve(F), F) :- fluent(F).
add(_preserve(F), F) :- fluent(F).
preserving(_preserve(F)) :- fluent(F).

step(0..k-1).

pg_fluent(F,0) :- init(F).
pg_action(A,T) :- pg_fluent(F,T) : pre(A,F); action(A); step(T).
pg_fluent(F,T+1) :- add(A,F); pg_action(A,T); step(T).

holds(F,k) :- goal(F).

{happens(A,T) : add(A,F), pg_action(A,T)} >= 1 :- holds(F,T+1); step(T).

holds(F,T) :- happens(A,T); pre(A,F).

erased(F,T) :- happens(A,T); del(A,F); not pre(A,F).
used(F,T) :- happens(A,T); pre(A,F); not del(A,F).

:- {erased(F,T); used(F,T); happens(A,T):pre(A,F),del(A,F)} > 1; step(T); fluent(F).

% :~ happens(A,T); cost(A,C).[C,happens(A,T)]

do(A,T) :- happens(A,T); not preserving(A).

#show do/2.

action(_preserve(F)) :- fluent(F).
pre(_preserve(F), F) :- fluent(F).
add(_preserve(F), F) :- fluent(F).
preserving(_preserve(F)) :- fluent(F).

step(0..k-1).

holds(F,k) :- goal(F).

{do(A,T) : add(A,F)} >= 1 :- holds(F,T+1); step(T).

holds(F,T) :- do(A,T); pre(A,F).

:- holds(F,0); not init(F,0).

deleted(F,T) :- do(A,T); del(A,F); not pre(A,F).
used(F,T) :- do(A,T); pre(A,F); not del(A,F).

:- {deleted(F,T); used(F,T); do(A,T):pre(A,F),del(A,F)} > 1; step(T); fluent(F).

:~ do(A,T); cost(A,C).[C,do(A,T)]

happens(A,T) :- do(A,T); not preserving(A).

#show happens/2.

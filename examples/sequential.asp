step(0..k-1).

holds(F,0) :- init(F).

{happens(A,T) : action(A)} = 1 :- step(T).

:- happens(A,T); pre(A,F); not holds(F,T).

holds(F,T+1) :- happens(A,T); add(A,F); step(T).

holds(F,T+1) :- holds(F,T); not deleted(F,T); step(T).

deleted(F,T) :- happens(A,T); del(A,F).

:- goal(F); not holds(F,k).

:~ happens(A,T); cost(A,C).[C,happens(A,T)]

#show happens/2.

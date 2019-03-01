step(0..k-1).

holds(F,0) :- init(F).

allowed(A,T) :- action(A); step(T); holds(F,T) : pre(A,F).

{happens(A,T) : allowed(A,T)} <= 1 :- step(T).

holds(F,T+1) :- happens(A,T); add(A,F); step(T).

deleted(F,T) :- happens(A,T); del(A,F).

holds(F,T+1) :- holds(F,T); not deleted(F,T); step(T).

:- goal(F); not holds(F,k).

% :~ happens(A,T); cost(A,C).[C,happens(A,T)]

#show happens/2.

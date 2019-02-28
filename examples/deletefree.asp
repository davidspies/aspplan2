holds(F) :- init(F).

{happens(A)} :- holds(F) : pre(A,F); action(A).

holds(F) :- happens(A); add(A,F).

:- goal(F); not holds(F).

:~ happens(A); cost(A,C).[C,happens(A)]

#show happens/1.

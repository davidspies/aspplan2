visit(START) :- initial(START).

{travel(A,B) : adjacent(A,B)} <= 1 :- visit(A).

visit(B) :- travel(_, B).

:- not visit(V); vertex(V).

:~ travel(A,B); distance(edge(A,B),D).[D,edge(A,B)]

#show travel/2.

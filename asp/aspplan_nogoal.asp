#program base.
action(i__preserve(F)) :- fluent(F).
pre(i__preserve(F), F) :- fluent(F).
add(i__preserve(F), F) :- fluent(F).
preserving(i__preserve(F)) :- fluent(F).

pgFluent(F, 0) :- init(F).

#external holdsNext(F,0) : pgFluent(F,0).
holds(F, 0) :- holdsNext(F,0).
:- holds(F, 0); not init(F).
#external query(0).

#program step(t).

step(t) :- t > 0.

pgAct(A, t) :- pgFluent(F, t-1) : pre(A, F); action(A); step(t).
pgFluent(F, t) :- pgAct(A, t); add(A, F).

#external holdsNext(F,t) : pgFluent(F,t), step(t).
#external query(t) : step(t).
holds(F,t) :- holdsNext(F,t).

{happens(A,t) : add(A,F), pgAct(A,t)} >= 1 :- holds(F,t); step(t).
holdsNext(F,t-1) :- happens(A,t); pre(A,F).
:- holds(F,t); happens(A,t); del(A,F).

:- holds(F,t); holds(G,t); mutex(F,G).
:- {partHolds(P,t) : isPartitionIn(P,C); holds(F,t) : isSingletonPartIn(F,C)} > 1; isMultiClique(C).
partHolds(P,t) :- holds(F,t); inPart(F,P).

used(F,t) :- happens(A,t); pre(A,F); not del(A,F).
deleted(F,t) :- happens(A,t); del(A,F); not pre(A,F).
:- {happens(A,t) : pre(A,F), del(A,F); used(F,t); deleted(F,t)} > 1; pgFluent(F,t).

do(A,t) :- happens(A,t); not preserving(A).

:~ happens(A,t); cost(A,V).[V,A,t]

#show do/2.

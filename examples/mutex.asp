:- holds(F,T); holds(G,T); mutex(F,G).
:- {partHolds(P,T) : isPartitionIn(P,C); holds(F,T) : isSingletonPartIn(F,C)} > 1;
    isMultiClique(C); step(T).
partHolds(P,T) :- holds(F,T); inPart(F,P).

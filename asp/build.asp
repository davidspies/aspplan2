hasType(X, T) :- hasType(X, S); isSubtype(S, T).

fluentp(F) :- initp(F).
fluentp(F) :- actionp(A); addp(A,F).

needed(fluent(F)) :- goalp(F).
needed(action(A)) :- addp(A,F); not prep(A,F); fluent(F).
needed(fluent(F)) :- prep(A,F); action(A).

action(A) :- needed(action(A)); actionp(A).

deletable(F) :- action(A); delp(A,F); not addp(A,F).

fluent(F) :- needed(fluent(F)); fluentp(F); not initp(F).
fluent(F) :- needed(fluent(F)); fluentp(F); deletable(F).

init(F) :- initp(F); fluent(F).
goal(F) :- goalp(F); fluent(F).
goal(F) :- goalp(F); not initp(F).

pre(A,F) :- action(A); fluent(F); prep(A,F).
add(A,F) :- action(A); fluent(F); addp(A,F); not prep(A,F).
del(A,F) :- action(A); fluent(F); delp(A,F); not addp(A,F).

cost(A,S) :- action(A);
  S=#sum{V,I:costp(A,I,valueOf(T)),value(T,V); V,I:costp(A,I,constValue(V))};
  S > 0.

#show fluent/1.
#show action/1.
#show pre/2.
#show add/2.
#show del/2.
#show init/1.
#show goal/1.
#show cost/2.

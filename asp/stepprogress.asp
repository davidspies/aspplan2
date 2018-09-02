#program step(t).

:- not holds(F,t) : not holds(F,S-1), fluent(F); step(S); S <= t; useSuffix.

cleared(F,t) :- happens(A,t); del(A,F).
needed(F,t) :- do(A,t); pre(A,F).
:- do(A,t);
  holds(F,t-2) : pre(A,F);
  not cleared(G,t-1) : add(A,G),holds(G,t);
  not needed(H,t-1) : del(A,H).

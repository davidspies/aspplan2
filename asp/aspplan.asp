#include "aspplan_nogoal.asp".

#program base.

holds(F,0) :- goal(F); query(0).

#program step(t).

holds(F,t) :- goal(F); query(t).

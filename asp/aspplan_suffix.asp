#include "aspplan_nogoal.asp".
#include "stepprogress.asp".
#include "suffix.asp".

#program base.

holds(F,0) :- subgoal(F); query(0).

#program step(t).

holds(F,t) :- subgoal(F); query(t).

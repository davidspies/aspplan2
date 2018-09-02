nonmutex(F,G) :- init(F); init(G).
nonmutexAF(A,F) :- add(A,F).
nonmutexAF(A,F) :- nonmutex(F,G) : pre(A,G); action(A); fact(F); not del(A,F).
nonmutex(F,G) :- nonmutexAF(A,F); add(A,G).
nonmutex(F,G) :- nonmutexAF(A,G); add(A,F).

mutex(F,G) :- fact(F); fact(G); not nonmutex(F,G).

#show mutex/2.

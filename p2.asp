actionp(step1):-fluentp(p).
prep(step1,p):-actionp(step1).
delp(step1,p):-actionp(step1).
addp(step1,q):-actionp(step1).
actionp(step2):-fluentp(q).
prep(step2,q):-actionp(step2).
delp(step2,q):-actionp(step2).
addp(step2,p):-actionp(step2).
actionp(win):-fluentp(r).
prep(win,r):-actionp(win).
addp(win,g):-actionp(win).
initp(p).
goalp(g).



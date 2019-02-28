{in_bag(Obj)} :- is_object(Obj).

:- #sum{W,Obj : weight(Obj,W),in_bag(Obj)} > Cap; sack_capacity(Cap).

:~ in_bag(Obj); value(Obj,V).[-V,Obj]

#show in_bag/1.

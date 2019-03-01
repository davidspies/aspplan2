vertex(1..nverts).
edge(e(V,W)) :- vertex(V); vertex(W); V < W.
color(C) :- color_size(C,_).

{has_color(E,C) : color(C)} = 1 :- edge(E).

in_clique(V) | not_in_clique(V) :- vertex(V).
clique_color(C) : color(C).
edge_in_clique(e(V,W)) :- in_clique(V); in_clique(W).
clique_color(C) :- edge_in_clique(E); has_color(E,C).

not_counterexample :-
  {not_in_clique(_)} > nverts - S; color_size(C,S); clique_color(C).
not_counterexample :- {clique_color(C)} > 1.
not_counterexample :- in_clique(V); not_in_clique(V).

in_clique(V) :- not_counterexample; vertex(V).
not_in_clique(V) :- not_counterexample; vertex(V).
clique_color(C) :- not_counterexample; color(C).

:- not not_counterexample.

#show has_color/2.

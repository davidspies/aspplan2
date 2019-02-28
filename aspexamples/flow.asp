is_row(1..NR) :- nrows(NR).
is_col(1..NC) :- ncols(NC).
vertex(v(R,C)) :- is_row(R); is_col(C).
adjacent(v(R,C),v(R+1,C)) :- vertex(v(R,C)); vertex(v(R+1,C)).
adjacent(v(R,C),v(R-1,C)) :- vertex(v(R,C)); vertex(v(R-1,C)).
adjacent(v(R,C),v(R,C+1)) :- vertex(v(R,C)); vertex(v(R,C+1)).
adjacent(v(R,C),v(R,C-1)) :- vertex(v(R,C)); vertex(v(R,C-1)).

is_start(V) :- start(V,_).
is_end(V) :- end(V,_).

is_color(V,C) :- start(V,C).
is_color(V,C) :- to(W,V); is_color(W,C).
flow(V) :- is_color(V,_).

{to(V,W) : adjacent(V,W)} = 1 :- flow(V); not is_end(V).
:- {to(_,V)} > 1; vertex(V).
:- not to(W,V) : adjacent(W,V); not is_start(V); vertex(V).

:- is_color(V,D); end(V,C); C != D.

#show to/2.

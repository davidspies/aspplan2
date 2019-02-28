{color(Vert,Color) : is_color(Color)} = 1 :- is_vertex(Vert).

:- color(VL,Color); color(VR,Color); adjacent(VL,VR).

#show color/2.

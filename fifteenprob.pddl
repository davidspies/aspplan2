(define (problem fifteen)
  (:domain fifteen-puzzle)
  (:objects
    p11 p12 p13 p14
    p21 p22 p23 p24
    p31 p32 p33 p34
    p41 p42 p43 p44 - place
    s1 s2 s3 s4 s5 s6 s7 s8
    s9 s10 s11 s12 s13 s14 s15 - slider)
  (:init
    (adjacent p11 p12)
    (adjacent p11 p21)
    (adjacent p12 p13)
    (adjacent p12 p22)
    (adjacent p12 p11)
    (adjacent p13 p14)
    (adjacent p13 p23)
    (adjacent p13 p12)
    (adjacent p14 p24)
    (adjacent p14 p13)
    (adjacent p21 p11)
    (adjacent p21 p22)
    (adjacent p21 p31)
    (adjacent p22 p12)
    (adjacent p22 p23)
    (adjacent p22 p32)
    (adjacent p22 p21)
    (adjacent p23 p13)
    (adjacent p23 p24)
    (adjacent p23 p33)
    (adjacent p23 p22)
    (adjacent p24 p14)
    (adjacent p24 p34)
    (adjacent p24 p23)
    (adjacent p31 p21)
    (adjacent p31 p32)
    (adjacent p31 p41)
    (adjacent p32 p22)
    (adjacent p32 p33)
    (adjacent p32 p42)
    (adjacent p32 p31)
    (adjacent p33 p23)
    (adjacent p33 p34)
    (adjacent p33 p43)
    (adjacent p33 p32)
    (adjacent p34 p24)
    (adjacent p34 p44)
    (adjacent p34 p33)
    (adjacent p41 p31)
    (adjacent p41 p42)
    (adjacent p42 p32)
    (adjacent p42 p43)
    (adjacent p42 p41)
    (adjacent p43 p33)
    (adjacent p43 p44)
    (adjacent p43 p42)
    (adjacent p44 p34)
    (adjacent p44 p43)
    (at s1 p11)
    (at s2 p12)
    (at s3 p13)
    (at s4 p14)
    (at s5 p21)
    (at s6 p22)
    (at s7 p23)
    (at s8 p24)
    (at s9 p31)
    (at s10 p32)
    (at s15 p33)
    (at s11 p34)
    (at s12 p41)
    (at s13 p42)
    (at s14 p43)
    (at blank p44))
  (:goal (and
    (at s1 p11)
    (at s2 p12)
    (at s3 p13)
    (at s4 p14)
    (at s5 p21)
    (at s6 p22)
    (at s7 p23)
    (at s8 p24)
    (at s9 p31)
    (at s10 p32)
    (at s11 p33)
    (at s12 p34)
    (at s13 p41)
    (at s14 p42)
    (at s15 p43)
    (at blank p44)))
)

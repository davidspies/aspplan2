(define (problem tumble-prob1)
  (:domain tumble)
  (:objects s1 s2 s3 - spinner)
    (:init
      (fixed s0)
      (drops f1 f2)
      (drops f2 f3)
      (drops f3 f1)
      (next-position p1 p2)
      (next-position p2 p3)
      (next-position p3 p4)
      (next-position p4 p1)

      (at s1 p1)
      (next-spinner s0 s1)
      (unfixed s1)

      (at s2 p1)
      (next-spinner s1 s2)
      (unfixed s2)

      (at s3 p1)
      (next-spinner s2 s3)
      (unfixed s3)

      (last-spinner s3)

	    (= (total-cost) 0))
  (:goal (and (set f1)
        (set f2)
        (set f3)))
  (:metric minimize (total-cost)))

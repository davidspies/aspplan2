(define (domain fifteen-puzzle)
  (:requirements :strips :typing)
  (:types place slider)
  (:constants blank - slider)
  (:predicates
    (adjacent ?p1 ?p2 - place)
  	(at ?s - slider ?p - place))
  (:action move
  	:parameters (?s - slider ?p1 ?p2 - place)
  	:precondition (and (adjacent ?p1 ?p2) (at ?s ?p1) (at blank ?p2))
  	:effect (and (not (at ?s ?p1)) (not (at blank ?p2)) (at ?s ?p2) (at blank ?p1))
  )
)

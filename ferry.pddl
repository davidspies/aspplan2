;;; Ferry domain

(define (domain ferry-strips)
  (:requirements :strips)

  (:constants ferry)
  (:predicates (at ?car ?island)
	       (place ?island)
	       (car ?car)
	       (at-ferry ?island)
	       (empty-ferry)
	       (on ?car ?island))

  (:action board
	     :parameters (?car ?island)
	     :precondition (and (at ?car ?island)
				 (place ?island)
				 (car ?car)
				 (at-ferry ?island)
				 (empty-ferry))
	     :effect (and (on ?car ferry)
			   (not (at ?car ?island))
			   (not (empty-ferry))))
  (:action move
	     :parameters (?from ?to)
	     :precondition (and (at-ferry ?from)
				 (place ?from)
				 (place ?to))
	     :effect (and (at-ferry ?to)
			   (not (at-ferry ?from))))
  (:action debark
	     :parameters (?car ?island)
	     :precondition (and (on ?car ferry)
				 (car ?car)
				 (place ?island)
				 (at-ferry ?island))
	     :effect (and (not (on ?car ferry))
			   (at ?car ?island)
			   (empty-ferry))))

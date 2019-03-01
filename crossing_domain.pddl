;;; Crossing domain

(define (domain crossing-strips)
  (:requirements :strips :typing :action-costs)
  (:types person side)
  (:constants lantern - object side-a side-b - side)
  (:predicates (at ?p - person ?s - side)
  			   (lantern-at ?s - side)
  			   (slower-than ?p1 ?p2 - person))
  (:functions (crossing-time ?p - person)
  			  (total-cost))
  (:action cross-alone
  		 :parameters (?p - person ?from ?to - side)
  		 :precondition (and
  		 				(at ?p ?from)
  		 				(lantern-at ?from))
  		 :effect (and
  		 		  (at ?p ?to)
  		 		  (lantern-at ?to)
  		 		  (not (at ?p ?from))
  		 		  (not (lantern-at ?from))
  		 		  (increase (total-cost) (crossing-time ?p))))
  (:action cross-together
  		 :parameters (?p-slow ?p-fast - person ?from ?to - side)
  		 :precondition (and
  		 				(at ?p-slow ?from)
  		 				(at ?p-fast ?from)
  		 				(lantern-at ?from)
  		 				(slower-than ?p-slow ?p-fast))
  		 :effect (and
  		 		  (at ?p-slow ?to)
  		 		  (at ?p-fast ?to)
  		 		  (lantern-at ?to)
  		 		  (not (at ?p-slow ?from))
  		 		  (not (at ?p-fast ?from))
  		 		  (not (lantern-at ?from))
  		 		  (increase (total-cost) (crossing-time ?p-slow)))))

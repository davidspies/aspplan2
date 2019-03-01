;;; Crossing domain

(define (domain tumble)
  (:requirements :strips :typing :action-costs)
  (:types spinner flag position)
  (:constants p1 p2 p3 p4 - position s0 - spinner f1 f2 f3 - flag)
  (:predicates (at ?s - spinner ?p - position)
               (drops ?f1 ?f2 - flag)
               (set ?f - flag)
               (next-spinner ?s1 ?s2 - spinner)
               (last-spinner ?s - spinner)
               (next-position ?p1 ?p2 - position)
               (fixed ?s - spinner)
               (unfixed ?s - spinner))
  (:functions (total-cost))
  (:action spin-up
  		 :parameters (?s - spinner ?from ?to - position)
  		 :precondition (and
              (unfixed ?s)
  		 				(at ?s ?from)
              (next-position ?from ?to))
  		 :effect (and
  		 		  (not (at ?s ?from))
  		 		  (at ?s ?to)
  		 		  (increase (total-cost) 1)))
  (:action spin-down
  		 :parameters (?s - spinner ?from ?to - position)
  		 :precondition (and
              (unfixed ?s)
  		 				(at ?s ?from)
              (next-position ?to ?from))
  		 :effect (and
  		 		  (not (at ?s ?from))
  		 		  (at ?s ?to)
  		 		  (increase (total-cost) 1)))
  (:action fix
  		 :parameters (?prev-s ?s - spinner)
  		 :precondition (and
              (fixed ?prev-s)
              (next-spinner ?prev-s ?s)
              (unfixed ?s)
              (at ?s p3))
  		 :effect (and
              (fixed ?s)
              (not (unfixed ?s))))
  (:action set-false
      :parameters (?flag ?dropflag - flag ?ls - spinner)
      :precondition (and
              (drops ?flag ?dropflag)
              (fixed ?ls)
              (last-spinner ?ls))
      :effect (and
            (set ?flag)
            (not (set ?dropflag))
            (increase (total-cost) 1)))
  (:action set-true
      :parameters (?flag - flag ?ls - spinner)
      :precondition (and
              (fixed ?ls)
              (last-spinner ?ls))
      :effect (and
            (set ?flag)
            (increase (total-cost) 1000000))))

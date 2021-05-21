#lang racket

(require rackunit "../infrabel/infrabel.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUN THIS ALONGSIDE nmbs.rkt OR THIS WILL AUTOMATICALLY FAIL ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case
 "infrabel testing"
 (define infrabel (make-object infrabel% #f))
 (test-begin 
  (check-pred infrabel? infrabel)
  (send infrabel start-infrabel)
  (check-pred null? (send infrabel fetch-railway))
  (check-pred null? (send infrabel fetch-reservation-graph))
  (check-pred null? (send infrabel get-list-of-locos))
  (check-pred null? (send infrabel get-list-of-dblocks))
  (check-pred null? (send infrabel get-list-of-switches))
  ;; AT THIS POINT INITIATE NMBS WITH A CERTAIN SETUP
  ;; YOU HAVE THE AMOUNT OF SECONDS SPECIFIED IN THE ARG PART WITHIN (sleep <ARG>)
  ;; DO THIS OR THE TEST WILL AUTOMATICALLY FAIL
  (displayln "PICK A SETUP THROUGH THE APPLICATION")
  (sleep 10)
  (check-pred (λ (lst)
                (andmap (λ (pairs)
                          (and (pair? pairs)
                               (symbol? (first pairs))
                               (symbol? (second pairs))))
                        lst))(send infrabel fetch-railway))
  (check-pred (λ (lst)
                (andmap (λ (pairs)
                          (and (pair? pairs)
                               (symbol? (first pairs))
                               (symbol? (second pairs))))
                        lst))(send infrabel fetch-reservation-graph))
  (check-pred (λ (lst)
                (andmap (λ (SBP)
                          (and (symbol? (first SBP))
                               (boolean? (second SBP))
                               (integer? (third SBP))))
                        lst))
              (send infrabel get-list-of-switches))
  (check-pred (λ (lst)
                (andmap (λ (SBB)
                          (and (symbol? (first SBB))
                               (boolean? (second SBB))
                               (boolean? (third SBB))))
                        lst))
              (send infrabel get-list-of-dblocks))
  (check-pred null?
              (send infrabel get-list-of-locos))
  ;; YOU NOW HAVE TO MANUALLY ADD A TRAIN THROUGH THE NMBS APPLICATION
  ;; YOU HAVE 30 seconds to do this
  ;; IF YOU ADD MORE THAN 1 IT WILL FAIL
  (displayln "PLEASE ADD A TRAIN THROUGH THE APPLICATION")
  (sleep 30)
  (check-pred (λ(lst)
                (andmap (λ (loco)
                          (and (vector? loco)
                               (symbol? (vector-ref loco 0))
                               (boolean? (vector-ref loco 1))
                               (integer? (vector-ref loco 2))
                               (symbol? (vector-ref loco 3))
                               (boolean? (vector-ref loco 4))
                               (boolean? (vector-ref loco 5))
                               (boolean? (vector-ref loco 6))))lst))
              (send infrabel get-list-of-locos))
  (check-pred (λ (arg)(= 1 arg))(length (send infrabel get-list-of-locos)))
  (send infrabel set-loco-speed! (vector-ref (first(send infrabel get-list-of-locos))0) 50)
  (check-pred (λ (arg)(= 1 arg))(length (send infrabel get-list-of-locos)))
  (check-pred (λ(val)
                (and (integer? val)
                     (positive? val)))
              (vector-ref (first(send infrabel get-list-of-locos)) 2))
  ;; if this test succeeds it also proves that the set-loco-speed method works if the method was used.
  (check-pred (λ(lst)
                (ormap (λ(each)
                         (third each))lst))
              (send infrabel get-list-of-dblocks))
  (check-pred (λ (arg)(= 1 arg))(length (send infrabel get-list-of-locos)))
  (check-pred (λ (lst)
                (displayln (cons "the actual list " lst))
                (andmap (λ(id)
                          (let ((dblock (assoc id (send infrabel get-list-of-dblocks))))
                            (if dblock
                                (eq? (vector-ref(first(send infrabel get-list-of-locos))0)(second dblock))
                                (let ((switch (assoc id (send infrabel get-list-of-switches))))
                                  (eq? (vector-ref (first (send infrabel get-list-of-locos))0)(second switch))))))
                        lst))
              (vector-ref (first(send infrabel get-list-of-locos)) 4))
  (send infrabel set-switch-position! (first (first(send infrabel get-list-of-switches))) 2)
  (check-pred (λ(val)(= 2 val))(third (first (send infrabel get-list-of-switches))))
  ;; SET A DESTINATION THROUGH THE NMBS, WE EXPECT THAT NMBS CALCULATES THE TOTAL PATH
  ;; AND THAT THE DESTINATION IS ACCESIBLE THOUGH AUTOMATIC PATHING
  ;; YOU HAVE 20 SECONDS TO DO SO.
  (displayln "PLEASE SET THE DESTINATION THROUGH THE APPLICATION")
  (sleep 20)
  (check-pred (λ(val)(and (not (null? (vector-ref val 6)))
                          (symbol? (vector-ref val 5))))(first (send infrabel get-list-of-locos)))
  ;; ADD another train, set the destination of the train to the same as the first train
  (displayln "please add another train, set the destination")
  (sleep 30)
  ;; WE GIVE YOU 1 MINUTE TO TEST THE PATHING AND COLLISION PREVENTION.
  (displayln "you have 1 minute to prove it works")
  (sleep 60)
  (check-pred (λ(lst)
                (let ((val 0))
                  (for-each (λ(i)(when (third i)
                                   (set! val (+ val 1))))lst)
                  (= 2 val)))
              (send infrabel get-list-of-dblocks))
  ;; this test if both trains are on different locations
  (check-pred (λ(lst)
                (let ((loco1 (first lst))
                      (loco2 (second lst)))
                  (or (and (not (vector-ref loco1 5))(vector-ref loco2 5))
                      (and (vector-ref loco1 5)(not (vector-ref loco2 5))))))
              (send infrabel get-list-of-locos))
  ;; this tests if a train has reached its destination
  ))
#lang racket

(require rackunit "../locomotive_objects/locomotive.rkt")

(test-case
 "locomotive tests"
 (define loco (make-object locomotive%
                (string->symbol "test loco")
                'forward
                '1-1
                '1-2))
 (test-begin
  (test-pred "is a loco?" locomotive? loco)
  (check-eq? (send loco get-loco-id)(string->symbol "test loco"))
  (check-false (send loco made-reservations?))
  (send loco reserve! (list '1-1 '1-2 '1-3))
  (test-pred "is a list?" (λ(lst)
                            (andmap (λ (id)(symbol? id)) lst))
             (send loco made-reservations?))
  (send loco clear-reservations!)
  (check-false (send loco made-reservations?))
  (check-false (send loco manual?))
  (send loco manual-override #t)
  (check-true (send loco manual?))
  (test-pred "is zero?" zero? (send loco get-speed))
  (send loco set-speed! 100)
  (test-pred "is positive?" positive? (send loco get-speed))
  (check-eq? (send loco get-speed) 100)
  (check-eq? (send loco get-previous-location) '1-2)
  (check-eq? (send loco get-direction) 'forward)
  (send loco set-direction! 'backwards)
  (check-eq? (send loco get-direction) 'backwards)
  (check-false (send loco get-destination))
  (send loco set-destination! '1-5)
  (check-eq? (send loco get-destination) '1-5)
  (check-eq? (send loco get-location) '1-1)
  (send loco set-location! '1-5)
  (check-eq? (send loco get-previous-location) '1-1)
  (check-eq? (send loco get-location) '1-5))
  (send loco set-previous-location! 'S-2)
  (check-eq? (send loco get-previous-location) 'S-2)
  (check-false (send loco get-path))
  (send loco set-path! (list (list '1-1 '1-2 '1-3 '1-4)(list '1-5 '1-6 '1-7 '1-8)))
  (test-pred "list correctly formed?" (λ (lst)
                                        (andmap (λ (sublst)
                                                  (andmap (λ (el)
                                                            (symbol? el))
                                                          sublst))
                                                lst))
             (send loco get-path)))
  
 
#lang racket

(require rackunit "../railway_objects/track.rkt")

(test-case
 "track ADT"
 (define track (make-object track% 'U-1 (list 'S-1 '1-1)))
 (test-begin
  (test-pred "object check" track? track)
  (check-eq? (send track get-track-id) 'U-1 "Track ID check")
  (test-pred "vector check for links" vector? (send track track-links))
  (check-eq? (send track nr-of-links) 2)
  (check-eq? (vector-ref (send track track-links) 0) 'S-1 "First linked ID")
  (check-eq? (vector-ref (send track track-links) 1) '1-1 "second linked ID")
  (send track set-links! (list 'S-2 '1-2 'S-3))
  (check-eq? (send track nr-of-links) 3)
  (check-eq? (vector-ref (send track track-links) 0) 'S-2 "Incorrectly set! first linked ID")
  (check-eq? (vector-ref (send track track-links) 1) '1-2 "incorrectly set! second linked ID")
  (check-eq? (vector-ref (send track track-links) 2) 'S-3 "incorrectly set! third linked ID")
  (let ((res (send track links-map (λ (id)"test"))))
    (check-eq? (vector-ref res 0) "test")
    (check-eq? (vector-ref res 1) "test"))
  (check-false (send track reserved?))
  (send track reserve! (string->symbol "test train"))
  (check-eq? (send track reserved?) (string->symbol "test train"))
  (send track cancel-reservation!)
  (check-false (send track reserved?))))
  
  

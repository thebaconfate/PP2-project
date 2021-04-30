#lang racket

(require "track.rkt")
(provide dblock% dblock?)

(define dblock% (class track%
                  (inherit-field id links)
                  (inherit get-track-id track-links reserve! reserved? cancel-reservation!)
                  (define occupancy #f)
                  (define last-train-id #f)
                  (super-new)

                  ;--------------------- private procedures
                  
                  
                  ;--------------------- public procedures

                  ; returns false or id of train that occupies it
                  (define/public (occupied?)occupancy)

                  ; changes occupancy to train-id and clears reservation
                  (define/public (occupy! train-id)
                    (cancel-reservation!)
                    (vacant!)
                    (set! occupancy train-id))

                  ; changes occupancy to #f (not occupied)
                  (define/public (vacant!)
                    (set! last-train-id occupancy)
                    (set! occupancy #f))))

; checks if the object is a dblock%
(define (dblock? object)
  (is-a? object dblock%))

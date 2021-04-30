#lang racket

(require "track.rkt")

(provide switch% switch?)

(define switch% (class track%
                  (inherit-field id links)
                  (inherit get-track-id track-links reserved? reserve! cancel-reservation!)

                  ;; a few abstractions
                  (define status-idx 1)
                  (define merge-idx 0)
                  (super-new)
                  ;--------------------- private procedures

                  (define/private (read-from-links idx)
                    (vector-ref links idx))
                  
                  
                  ;;--------------------- public procedures

                  ;; returns the index of the linked status
                  (define/public (get-status) status-idx)

                  ;; changes status
                  (define/public (set-status! new-status-idx)
                    (when (<= new-status-idx (- (vector-length links)1))
                      (set! status-idx new-status-idx)))

                  ;; returns the id of the bottom of the Y switch 
                  (define/public (get-merge-track)
                    (read-from-links merge-idx))

                  ;; returns the currently linked track id
                  (define/public (get-linked-track)
                    (read-from-links status-idx))))
                  

; checks if an object is a switch
(define (switch? object)
  (is-a? object switch%))

#lang racket

(provide track% track?)

(define track% (class object%
                 (init-field id links)
                 (define reserved #f)
                 (super-new)

                 ;; returns the track-id
                 (define/public (get-track-id) id)

                 ;; returns the vector containing the linked tracks
                 (define/public (track-links) links)

                 ;; updates the linked tracks with a new vector containing the new links, expects a list
                 (define/public (set-links list-of-new-links)
                   (set! links (list->vector list-of-new-links)))

                 ;; uses a proc on each element of the vector containing the links
                 (define/public (links-map proc)
                   (vector-map proc links))

                 ;; returns false or id that reserved it
                 (define/public (reserved?)reserved)
                 
                 ;; changes state of reservation
                 (define/public (reserve! train-id)
                   (set! reserved train-id))
                 
                 ;; clears the reservation))
                 (define/public (cancel-reservation!)
                   (set! reserved #f))

                 ;; destructively uses a proc on each element of the vector containing the links
                 (define/public (links-map! proc)
                   (vector-map! proc links))

                 (define/public (nr-of-links)
                   (vector-length links))

                 ;; sets the links to a vector
                 (set! links (list->vector links))))

(define (track? object)
  (is-a? object track%))
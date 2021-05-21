#lang racket

(require rackunit "../railway/railway.rkt")
(require graph)
(require "../railway_objects/track.rkt")
(require "../railway_objects/switch.rkt")
(require "../railway_objects/detection_block.rkt")
(require "../locomotive_objects/locomotive.rkt")

(test-case
 "railway testing"
 (define railway (make-object railway%))
 (test-begin
  (check-true (railway? railway))
  (define (add-dblock! track-id track-links)
    (send railway add-track! (send railway make-dblock track-id track-links)))
  
  (define (add-switch! track-id track-links)
    (send railway add-track! (send railway make-switch track-id track-links)))
  
  (define (add-track! track-id track-links)
    (send railway add-track! (send railway make-track track-id track-links)))
  (add-dblock! 'D1 (list 'D2 'T1))
  (add-dblock! 'D2 (list 'D3 'D2))
  (add-dblock! 'D3 (list 'D4 'D2))
  (add-dblock! 'D4 (list 'D3 'S1))
  (add-dblock! 'D5 (list 'D6 'T2))
  (add-dblock! 'D6 (list 'D5 'S2))
  (add-dblock! 'D7 (list 'S3 'S2))
  (add-dblock! 'D8 (list 'S3 'D9))
  (add-dblock! 'D9 (list 'D8))
  
  (add-switch! 'S1 (list 'D4 'T2 'T3))
  (add-switch! 'S2 (list 'T1 'D6 'D7))
  (add-switch! 'S3 (list 'T3 'D7 'D8))
  
  (add-track! 'T1 (list 'D1 'S2))
  (add-track! 'T2 (list 'S1 'D5))
  (add-track! 'T3 (list 'S1 'S3))
  (add-vertex! (send railway get-dblock-graph) 'D1)
  (add-vertex! (send railway get-dblock-graph) 'D2)
  (add-vertex! (send railway get-dblock-graph) 'D3)
  (add-vertex! (send railway get-dblock-graph) 'D4)
  (add-vertex! (send railway get-dblock-graph) 'D6)
  (add-vertex! (send railway get-dblock-graph) 'D7)
  (add-vertex! (send railway get-dblock-graph) 'D8)
  (add-vertex! (send railway get-dblock-graph) 'D9)
  (add-edge! (send railway get-dblock-graph) 'D1 'D2)
  (add-edge! (send railway get-dblock-graph) 'D1 'D6)
  (add-edge! (send railway get-dblock-graph) 'D1 'D7)
  (add-edge! (send railway get-dblock-graph) 'D2 'D3)
  (add-edge! (send railway get-dblock-graph) 'D3 'D4)
  (add-edge! (send railway get-dblock-graph) 'D4 'D5)
  (add-edge! (send railway get-dblock-graph) 'D4 'D7)
  (add-edge! (send railway get-dblock-graph) 'D4 'D8)
  (add-edge! (send railway get-dblock-graph) 'D5 'D6)
  (add-edge! (send railway get-dblock-graph) 'D8 'D9)

  (check-true (andmap (λ(pairs)(and (symbol? (car pairs))
                                    (or (track? (cdr pairs))
                                        (switch? (cdr pairs))
                                        (dblock? (cdr pairs)))))
                      (send railway get-list-of-tracks)))
  (check-true (track? (send railway get-track 'T1)))
  (check-true (dblock? (send railway get-track 'D1)))
  (check-true (switch? (send railway get-track 'S1)))

  (send railway remove-track! 'T1)
  (check-false (send railway get-track 'T1))
  (add-track! 'T1 (list 'D1 'S2))
  (let ((res '())
        (lst (send railway get-list-of-tracks)))
    (send railway for-each-track (λ (id obj)(set! res (cons (send obj get-track-id) res))))
    (set! res (reverse res))
    (check-true (and (andmap (λ (id)(if (findf (λ (pairs)(eq? (car pairs) id)) lst)
                                   #t
                                   #f)) res)
                     (andmap (λ (pairs)(if (findf (λ (id)(eq? (car pairs) id)) res)
                                           #t
                                           #f))lst))))
  (check-true (graph? (send railway get-railway-graph)))
  (check-true (graph? (send railway get-dblock-graph)))
  (define test-train (send railway make-locomotive (string->symbol "test loco")
                'forward
                'D1
                'D2))
  (check-true (locomotive? test-train))
  (send railway add-locomotive! test-train)
  (check-true (locomotive? (send railway get-locomotive (string->symbol "test loco"))))
  (let ((res '()))
    (send railway for-each-loco (λ (id obj)(set! res (cons (send obj get-loco-id) res))))
    (check-true (eq? (car res) (string->symbol "test loco"))))
  (send railway remove-locomotive! (string->symbol "test loco"))
  (check-false (locomotive? (send railway get-locomotive (string->symbol "test loco"))))))
  
  
                     
  
              
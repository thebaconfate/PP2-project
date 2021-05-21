#lang racket


(require graph)
(require "../railway_objects/switch.rkt")
(require "../railway_objects/detection_block.rkt")
(require "../railway_objects/track.rkt")
(require "../locomotive_objects/locomotive.rkt")

(provide railway% railway?)

(define railway% (class object% 
                   (define railway-graph (unweighted-graph/undirected '()))
                   (define reservations-block-graph (unweighted-graph/undirected '()))
                   (define track-segments (make-hash))
                   (define locomotives (make-hash))
                   (super-new)

                     ;--------------------- private methods

                   ;; procedure to add tracks and their edges to the railway
                   (define/private (add-track-to-railway! track-id track-object)
                     (add-vertex! railway-graph track-id)
                     (send track-object links-map (λ (link-id)(add-edge! railway-graph track-id link-id))))

                   ;; procedure to add tracks to the datastructure
                   (define/private (add-track-object! track-object)
                     (hash-set! track-segments (send track-object get-track-id) track-object))


                   

                   ;--------------------- public methods

                   (define/public (get-list-of-tracks)
                     (hash->list track-segments))

                   ;; returns the track object given the track-id
                   (define/public (get-track track-id)
                     (hash-ref track-segments track-id #f))

                   ;; makes a track obejct and returns it
                   (define/public (make-track track-id track-links)
                     (make-object track% track-id track-links))

                   ;; makes a switch object and returns it
                   (define/public (make-switch track-id track-links)
                     (make-object switch% track-id track-links))

                   ;; makes a dblock object and returns it
                   (define/public (make-dblock track-id track-links)
                     (make-object dblock% track-id track-links))


                   ;; procedure to add track, needs a track-object as input
                   (define/public (add-track! track-object)
                     (when (track? track-object)
                       (add-track-object! track-object)
                       (add-track-to-railway! (send track-object get-track-id) track-object)
                       track-object))

                   ;; procedure to remove a track, expects a track-id
                   (define/public (remove-track! track-id)
                     (hash-remove! track-segments track-id)
                     (remove-vertex! railway-graph track-id))

                   ;; procedure to use a procedure on each track-segment
                   (define/public (for-each-track proc)
                     (hash-for-each track-segments proc))

                   ;; procedure to use a procedure on each linked track of a track segment
                   (define/public (for-each-link proc track-id)
                     (send (get-track track-id) links-map (λ (link)
                                                            (proc (get-track link)))))
                   
                   ;; returns the railway graph
                   (define/public (get-railway-graph) railway-graph)

                   ;; returns the graph with paths only between reservation blocks
                   (define/public (get-dblock-graph) reservations-block-graph)

                   ;; adds a new locomotive to the hashmap
                   (define/public (make-locomotive loco-id dir loc prev)
                     (make-object locomotive% loco-id dir loc prev))                        
                                      
                   ;; adds a locomotive to the hashmap
                   (define/public (add-locomotive! locomotive-object)
                     (when (locomotive? locomotive-object)
                       (hash-set! locomotives (send locomotive-object get-loco-id) locomotive-object)))

                   ;; removes a locomotive from the hashmap
                   (define/public (remove-locomotive! locomotive-id)
                     (hash-remove! locomotives locomotive-id))

                   ;; fetches a locomotive object from the hashmap
                   (define/public (get-locomotive locomotive-id)
                     (hash-ref locomotives locomotive-id #f))

                   (define/public (for-each-loco proc)
                     (hash-for-each locomotives proc))))

(define (railway? object)
  (is-a? object railway%))




             
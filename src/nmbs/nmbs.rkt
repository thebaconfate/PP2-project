#lang racket


(require graph)
(require "../gui/gui.rkt")
(require "../railway/railway.rkt")
(require "../other/other-procedures.rkt")
(require "../railway_objects/segment_checks.rkt")
(require "../tcp/help-procs.rkt")

(provide nmbs%)

(define nmbs% (class object%
                (init-field hostname)
                (define gui (new gui% [frame-width 900][frame-height 600][main-spacing 50]))
                (define-values (in out) (tcp-connect hostname 9000))
                (define railway (make-object railway%))
                (define paths (make-hash))
                (define update #f)
                (super-new)
                
                                
                (define/private (sync-all-with-infrabel)
                  (let ((railway-graph (send-and-receive in out (list 'get-railway-graph)))
                        (reservation-graph (send-and-receive in out (list 'get-reservation-graph)))
                        (dblocks (send-and-receive in out (list 'get-list-of-dblocks)))
                        (switches (send-and-receive in out (list 'get-list-of-switches)))
                        (locos (send-and-receive in out (list 'get-list-of-locos))))
                    (for-each (λ (edge)
                                (add-edge! (send railway get-railway-graph) (first edge)(second edge)))
                              railway-graph)
                    (for-each (λ (edge)
                                (add-edge! (send railway get-dblock-graph)(first edge)(second edge)))
                              reservation-graph)
                    (for-each (λ (dblock+)
                                ((λ (dblock reserved occupied)
                                   (send railway add-track! (send railway make-dblock dblock
                                                                  (get-neighbors (send railway get-railway-graph)
                                                                                 dblock)))
                                   (send (send railway get-track dblock) occupy! occupied)
                                   (send (send railway get-track dblock) reserve! reserved))(first dblock+)
                                                                                            (second dblock+)
                                                                                            (third dblock+)))
                              dblocks)
                    (for-each (λ (switch+)
                                ((λ (switch reserved status)
                                   (send railway add-track! (send railway make-switch switch
                                                                  (get-neighbors (send railway get-railway-graph) switch)))
                                   (send (send railway get-track switch) set-status! status))(first switch+)
                                                                                             (second switch+)
                                                                                             (third switch+)))                              
                              switches))
                  (update-switches!)
                  (update-dblocks!))



                (define/public (update-dblocks-data!)
                  (let ((dblocks (send-and-receive in out (list 'get-list-of-dblocks))))
                    (for-each (λ (dblock+)
                                (let ((id (first dblock+))
                                      (reserved (second dblock+))
                                      (occupied (third dblock+)))
                                  (unless (and (eq? reserved (send (send railway get-track id) reserved?))
                                               (eq? occupied (send (send railway get-track id) occupied?)))
                                    (send (send railway get-track id) reserve! reserved)
                                    (send (send railway get-track id) occupy! occupied))))
                              dblocks)))
                                  

                (define/public (update-dblocks!)
                  (let ((dblocks '()))
                    (send railway for-each-track (λ (id track)
                                                   (when (dblock? track)
                                                     (set! dblocks (cons id dblocks)))))
                    (set! dblocks (sort dblocks sort-id-proc))
                    (for-each (λ (id)
                                (let* ((obj (send railway get-track id))
                                       (occu (send obj occupied?))
                                       (rese (send obj reserved?)))
                                  (send gui draw-dblock-info! id
                                        (if occu
                                            (symbol->string occu)
                                            "Vacant")
                                        (if rese
                                            (symbol->string rese)
                                            "Vacant"))))
                              dblocks)))

                (define/private (update-switch-data!)
                  (let ((dblocks (send-and-receive in out (list 'get-list-of-switches))))
                    (for-each (λ (dblock+)
                                (let ((id (first dblock+))
                                      (reserved (second dblock+))
                                      (occupied (third dblock+)))
                                  (unless (and (eq? reserved (send (send railway get-track id) reserved?))
                                               (eq? occupied (send (send railway get-track id) occupied?)))
                                    (set! update #t)
                                    (send (send railway get-track id) set-status! occupied)
                                    (send (send railway get-track id) reserve! reserved))))
                              dblocks)))
                
                (define/private (update-switches!)
                  (let ((switches '()))
                    (send railway for-each-track (λ (id track)
                                                   (when (switch? track)
                                                     (set! switches (cons id switches)))))
                    (set! switches (sort switches sort-id-proc))
                    (for-each (λ (id)
                                (let* ((obj (send railway get-track id))
                                       (status (send obj get-status))
                                       (res (send obj reserved?)))
                                  (send gui draw-switches-info! id status res
                                        (λ (new-position)
                                          (send-message out (list 'set-switch-position! id (+ new-position 1)))
                                          (send obj set-status! new-position)))))
                              switches)))

                (define/private (make-parent-proc parents-hash)
                  (λ (child)(hash-ref parents-hash child)))

                (define/private (set-loco-speed! id spd)
                  (send (send railway get-locomotive id) set-speed! spd)
                  (send gui set-loco-speed! id spd))

                (define/private (set-dblock-occupied! dblock-id loco-id)
                  (send (send railway get-track dblock-id) occupy! loco-id)
                  (send (send railway get-locomotive loco-id) set-location! dblock-id)
                  (send gui set-dblock-occupied! dblock-id loco-id))

                (define/private (free! dblock-id)
                  (send gui free! dblock-id))

                (define/private (cancel! lst)
                  (for-each (λ (track-id)
                              (let ((track (send railway get-track track-id)))
                                (cond
                                  ((dblock? track)(send gui free-dblock! track-id))
                                  ((switch? track)(send gui free-switch! track-id)))))
                            lst))
                  
                (define/private (reserve! loco-id lst)
                  (for-each (λ (track-id)
                              (let ((track (send railway get-track track-id)))
                                (cond
                                  ((dblock? track)(send gui reserve-dblock! track-id loco-id))
                                  ((switch? track)(send gui reserve-switch! track-id loco-id)))))
                            lst))
                (define/private (shortest-path graph source destination)
                  (define-values (distance parents)(dijkstra graph source))
                  (define parent (make-parent-proc parents))
                  (path-to-source parents source destination))
                    
                (define/private (calc-path-between-dblocks loco-obj route)
                  (let looptieloop ((total-path '())
                                    (remainder route))
                    (if (< (length remainder) 2)
                        (begin
                          (displayln (reverse total-path))
                          (reverse total-path))
                        (looptieloop (cons (shortest-path (send railway get-railway-graph)
                                                                 (car remainder)
                                                                 (cadr remainder))
                                           total-path)
                                     (cdr remainder)))))
                
                (define/private (calc-route loco-id destination)
                  (let ((loco-obj (send railway get-locomotive loco-id)))
                    (if (eq? destination (send loco-obj get-location))
                        (displayln (send gui set-to-not-set loco-id))
                        (let ((dblocks-route (fewest-vertices-path (send railway get-dblock-graph)
                                                                   (send loco-obj get-location)
                                                                   destination)))
                          (if dblocks-route
                              (begin
                                (displayln dblocks-route)
                                (hash-set! paths (send loco-obj get-loco-id) dblocks-route)
                                (send-message out (list 'set-loco-destination! loco-id destination (calc-path-between-dblocks loco-obj dblocks-route))))
                              (begin
                                (displayln dblocks-route)
                                (displayln (send gui set-to-not-set loco-id))))))))
                            

                (define/private (tcp-handler)
                  (unless (port-closed? in)
                    (when (char-ready? in)
                      (let ((msg (read in)))
                        (displayln msg)
                        (if (eof-object? msg)
                            (begin (close-input-port in)
                                   (close-output-port out))
                            (match msg
                              [(list 'recalculate-path _ _)(calc-route (second msg)(third msg))
                                                           (send gui add-log! (string-append "Recalculated path of "
                                                                                             (symbol->string (second msg))
                                                                                             " with new location "
                                                                                             (symbol->string (third msg))))]
                              [(list 'halted _)(set-loco-speed! (second msg) 0)
                                               (send gui halt! (second msg))
                                               (send gui add-log! (string-append "Set train speed of "
                                                                                 (symbol->string (second msg))
                                                                                 " to 0."))]
                              [(list 'occupy! _ _)(set-dblock-occupied! (third msg)(second msg))
                                                  (send gui add-log! (string-append "Set detection block "
                                                                                    (symbol->string (third msg))
                                                                                    " occupancy to "
                                                                                    (symbol->string (second msg))))]
                              [(list 'free! _)(free! (second msg))
                                              (send gui add-log! (string-append "Set detection block "
                                                                                (symbol->string (second msg))
                                                                                " to free"))]
                              [(list 'reserved-tracks _ _)(reserve! (second msg)(third msg))
                                                          (send gui add-log! (string-append "Reserved the following tracks: "
                                                                                            (string-join (map (λ(id)
                                                                                                                (symbol->string id))
                                                                                                              (third msg)))
                                                                                            " for "
                                                                                            (symbol->string (second msg))))]
                              [(list 'cancel-tracks _)(cancel! (second msg))
                                                      (send gui add-log! (string-append "Cleared reservations for the following tracks: "
                                                                                        (string-join (map (λ(id)
                                                                                                            (symbol->string id))
                                                                                                          (second msg)))))]
                              [(list 'reached-destination _)(send gui set-to-not-set (second msg))
                                                            (send gui add-log! (string-append (symbol->string (second msg))
                                                                                              " reached destination."))]
                              [(list 'set-switch-position! _ _)(send gui set-switch-position! (second msg)(- (third msg) 1))
                                                               (send gui add-log! (string-append "Set switch position of "
                                                                                                 (symbol->string (second msg))
                                                                                                 " to "
                                                                                                 (number->string (third msg))))]
                              [(list 'set-route! _ _)(send gui set-route! (second msg)(third msg))
                                                     (send gui add-log! (string-append "Set route of "
                                                                                       (symbol->string (second msg))
                                                                                       " to "
                                                                                       (if (string? (third msg))
                                                                                           (third msg)
                                                                                           (string-join (map (λ(id)
                                                                                                           (symbol->string id))
                                                                                                         (third msg))))))]
                              [(list 'add-log! _)(send gui add-log! (second msg))] 
                              [_ (display  "incorrect message syntax: " )
                                 (displayln msg)]))))))
                    


                (define (main-loop)
                  (tcp-handler)
                  (main-loop))
               
         
                                               
                (define/public (start-nmbs)
                  (send gui init)
                  (send gui init-tab-1 (λ ()
                                         (send-message out (list 'close-connection))
                                         (close-output-port out)
                                         (close-input-port in)
                                         (send gui exit)))
                  (send gui init-tab-3
                        (λ (id prev current)
                          (if (not (send railway get-locomotive id))
                              (if (and (dblock? (send railway get-track prev))
                                       (dblock? (send railway get-track current)))
                                  (begin (send railway add-locomotive! (send railway make-locomotive
                                                                             id 'forward current prev))
                                         (send-message out (list 'add-loco! id prev current))
                                         (let ((loco (send railway get-locomotive id)))
                                           (send gui add-locomotive!
                                                 id
                                                 (send loco get-speed)
                                                 (λ (new-speed)
                                                   (send (send railway get-locomotive id) set-speed! new-speed)
                                                   (send-message out (list 'set-loco-speed! id new-speed)));; spd cback
                                                 (send loco get-direction)
                                                 (λ (new-direction)
                                                   (send (send railway get-locomotive id) set-direction! new-direction)
                                                   (send-message out (list 'set-loco-direction!
                                                                           id
                                                                           (if (zero? new-direction)
                                                                               'forward
                                                                               'backwards))));; dir cback
                                                 (send loco manual?)
                                                 (λ (new-manual)
                                                   (send (send railway get-locomotive id) manual-override (not (zero? new-manual)))
                                                   (send-message out (list 'set-manual-mode! id (not (zero? new-manual)))));; manual cback
                                                 (send loco get-location)
                                                 (send loco get-destination)
                                                 (λ (new-destination)
                                                   (if (eq? new-destination "Not set")
                                                       (send-message out (list 'set-loco-destination! id #f #f))
                                                       (calc-route id (string->symbol new-destination)))))))
                                  #f)
                              #f)) 
                        (λ (id)
                          (if (send railway get-locomotive id)
                              (begin (send railway remove-locomotive! id)
                                     (send-message out (list 'remove-loco! id))
                                     (send gui remove-locomotive! id)
                                     #t)
                              #f)))
                  (send gui start-gui (if (car(send-and-receive in out (list 'hardware?)))
                                          (list (cons "Hardware setup" (λ ()
                                                                         (send-message out (list 'initialize-infrabel
                                                                                                 'setup-hardware))
                                                                         (sync-all-with-infrabel))))
                                          (list (cons "Hardware setup" (λ ()
                                                                         (send-message out (list 'initialize-infrabel
                                                                                                 'setup-hardware))
                                                                         (sync-all-with-infrabel)))
                                                (cons "Setup straight" (λ ()
                                                                         (send-message out (list 'initialize-infrabel
                                                                                                 'setup-straight))
                                                                         (sync-all-with-infrabel)))
                                                (cons "Setup straight with switch" (λ ()
                                                                                     (send-message
                                                                                      out
                                                                                      (list 'initialize-infrabel
                                                                                            'setup-straight-with-switch))
                                                                                     (sync-all-with-infrabel)))
                                                (cons "Setup loop" (λ ()
                                                                     (send-message out
                                                                                   (list 'initialize-infrabel
                                                                                         'setup-loop))
                                                                     (sync-all-with-infrabel)))
                                                (cons "Setup loop and switches" (λ ()
                                                                                  (send-message
                                                                                   out
                                                                                   (list 'initialize-infrabel
                                                                                         'setup-loop-and-switches))
                                                                                  (sync-all-with-infrabel))))))
                  (thread main-loop))))

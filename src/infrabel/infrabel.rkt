#lang racket

(require racket/tcp)
(require graph)
(require "../other/other-procedures.rkt")
(require "../railway/railway.rkt")
(require "../railway_objects/segment_checks.rkt")
(require (prefix-in sim: "../simulator/interface.rkt"))
(require (prefix-in hw: "../hardware-library/interface.rkt"))
(require "../tcp/help-procs.rkt")


(provide infrabel% infrabel?)

(define infrabel% (class object%
                    (init-field hardware-active)
                    (define server (tcp-listen 9000))
                    (define-values (in out)(tcp-accept server))
                    (define railway (new railway%))
                  
                    ;; abstractions for hardware/simulator procedures, will be set!'ed to the appropiate
                    ;; procedures depending on the hardware-active argument
                    (define start #f)
                    (define stop #f)
                    (define add-loco #f)
                    (define get-loco-spd #f)
                    (define set-loco-spd! #f)
                    (define get-loco-detection-block #f)
                    (define get-switch-pos #f)
                    (define set-switch-pos! #f)

                    ;; simulator only procedures
                    (define remove-loco #f)
                    (define setup-hardware #f)
                    (define setup-straight #f)
                    (define setup-straight-with-switch #f)
                    (define setup-loop #f)
                    (define setup-loop-and-switches #f)
                    (define get-detection-block-ids #f)
                    (define get-switch-ids #f)
                    (super-new)
                                        
                    ;--------------------- private methods
            
                    ;; binds definitions to either hardware or simulator interface definitions
                    (define/private (setup-infrabel)
                      (if hardware-active
                          (begin
                            (set! start hw:start)
                            (set! stop hw:stop)
                            (set! add-loco hw:add-loco)
                            (set! get-loco-spd hw:get-loco-speed)
                            (set! set-loco-spd! hw:set-loco-speed!)
                            (set! get-loco-detection-block hw:get-loco-detection-block)
                            (set! get-switch-pos hw:get-switch-position)
                            (set! set-switch-pos! hw:set-switch-position!))
                          (begin
                            (set! start sim:start)
                            (set! stop sim:stop)
                            (set! remove-loco sim:remove-loco)
                            (set! add-loco sim:add-loco)
                            (set! get-loco-spd sim:get-loco-speed)
                            (set! set-loco-spd! sim:set-loco-speed!)
                            (set! get-loco-detection-block sim:get-loco-detection-block)
                            (set! get-switch-pos sim:get-switch-position)
                            (set! set-switch-pos! sim:set-switch-position!)
                            (set! setup-hardware sim:setup-hardware)
                            (set! setup-straight sim:setup-straight)
                            (set! setup-straight-with-switch sim:setup-straight-with-switch)
                            (set! setup-loop sim:setup-loop)
                            (set! setup-loop-and-switches sim:setup-loop-and-switches)
                            (set! get-detection-block-ids sim:get-detection-block-ids)
                            (set! get-switch-ids sim:get-switch-ids))))
                    (setup-infrabel)

                    ;; a few abstractions to simplify adding railway segments
                    (define/private (add-dblock! track-id track-links)
                      (send railway add-track! (send railway make-dblock track-id track-links)))
                    
                    (define/private (add-switch! track-id track-links)
                      (send railway add-track! (send railway make-switch track-id track-links)))

                    (define/private (add-track! track-id track-links)
                      (send railway add-track! (send railway make-track track-id track-links)))

                    ;; reserves tracks with a given list of tracks
                    (define/private (reserve-tracks! loco-id list-of-tracks)
                      (for-each (λ (track-id)
                                  (send (send railway get-track track-id) reserve! loco-id))
                                list-of-tracks))

                    ;; cancels reservations for a given list of tracks
                    (define/private (cancel-reservations! list-of-tracks)
                      (unless (or (null? list-of-tracks) (not list-of-tracks))
                        (send-message out (list 'cancel-tracks list-of-tracks))
                        (for-each (λ (track-id)
                                    (send (send railway get-track track-id) cancel-reservation!)) list-of-tracks)))

                    
                    ;; sets the locomotive speed internally and on the hardware
                    (define/private (set-speed-loco-obj! loco-obj new-speed)
                      (send loco-obj set-speed! new-speed)
                      (if (eq? 'forward (send loco-obj get-direction))
                          (set-loco-spd! (send loco-obj get-loco-id) new-speed)
                          (set-loco-spd! (send loco-obj get-loco-id)(- new-speed))))
                          
                      
                    ;; checks if a list of tracks is free for the locomotive to reserver
                    (define/private (check-if-free-for-loco lst-of-path loco-obj)
                      (define (free? track)
                        (not (send track reserved?)))
                      (define (not-occupied? dblock)
                        (not (send dblock occupied?)))
                      (let ((loco-id (send loco-obj get-loco-id))
                            (manual-mode (send loco-obj manual?)))
                        (andmap (λ (track-id)
                                  (let ((track (send railway get-track track-id)))
                                    (if (free? track)
                                        (if (dblock? track)
                                            (if manual-mode
                                                manual-mode
                                                (if (not-occupied? track);; do the thing
                                                    #t
                                                    (eq? (send track occupied?)loco-id)))
                                            #t)
                                        (eq? (send track reserved?) loco-id))))
                                lst-of-path)))
                    


                    ;; stops the train
                    (define/private (halt! loco-obj)
                      (send loco-obj set-speed! 0)
                      (set-loco-spd! (send loco-obj get-loco-id) 0)
                      (send-message out (list 'halted (send loco-obj get-loco-id))))
                            

                    
                    ;; algoritm to make reservations or stop the locomotive on manual navigation
                    (define/private (make-reservations! loco-obj)
                      (let ((railway-graph (send railway get-railway-graph))
                            (previous (send loco-obj get-previous-location))
                            (current (send loco-obj get-location))
                            (potential-path '())
                            (red-flag #f))
                        (define-values (distance parents)(dijkstra railway-graph current))
                        (define parent (make-parent-proc parents))
                        (when previous 
                          (let find-red-flag ((element previous))
                            (if (has-edge? railway-graph element current)
                                (set! red-flag element)
                                (find-red-flag (parent element)))))
                        (for-each (λ (neighbor)
                                    (unless (eq? neighbor previous)
                                      (let ((path (path-to-source parents current neighbor)))
                                        (unless (ormap (λ (segment)(eq? segment red-flag)) path)
                                          (set! potential-path (append path potential-path))))))                        
                                  (get-neighbors (send railway get-dblock-graph) current))
                        (set! potential-path (remove-duplicates potential-path))
                        (if (check-if-free-for-loco potential-path loco-obj)
                            (begin
                              (reserve-tracks! (send loco-obj get-loco-id) potential-path)
                              (send loco-obj reserve! potential-path)
                              (send-message out (list 'reserved-tracks (send loco-obj get-loco-id) potential-path)))
                            (halt! loco-obj))))
    
                    
                    (define/private (invert-previous! loco-obj)
                      (if (send loco-obj made-reservations?)
                          (if (null? (send loco-obj made-reservations?))
                              (send loco-obj set-previous-location! #f)
                              (let find-new-previous ((reservations (send loco-obj made-reservations?)))
                                (if (dblock? (send railway get-track (car reservations)))
                                    (if (eq? (car reservations)(send loco-obj get-location))
                                        (find-new-previous (cdr reservations))
                                        (send loco-obj set-previous-location! (car reservations)))
                                    (find-new-previous (cdr reservations)))))
                          (let ((previous (send loco-obj get-previous-location))
                                (red-flag #f))
                            (define-values (distances parents)(dijkstra (send railway get-railway-graph)(send loco-obj get-location)))
                            (define parent (make-parent-proc parents))
                            (if previous
                                (begin (let find-red-flag ((element previous))
                                         (if (has-edge? (send railway get-railway-graph) element (send loco-obj get-location))
                                             (set! red-flag element)
                                             (find-red-flag (parent element))))
                                       (let find-new-previous ((neighbors (get-neighbors (send railway get-dblock-graph)(send loco-obj get-location))))
                                         (unless (null? neighbors)
                                           (unless (eq? (car neighbors) previous)
                                             (let ((path (path-to-source parents (send loco-obj get-location)(car neighbors))))
                                               (if (ormap (λ (segment)(eq? segment red-flag)) path)
                                                   (find-new-previous (cdr neighbors))
                                                   (send loco-obj set-previous-location! (car neighbors))))))))
                                (if (< (length (get-edges (send railway get-railway-graph)(send loco-obj get-location))) 2)
                                    (send loco-obj set-previous-location! (car (get-neighbors (send railway get-railway-graph)(send loco-obj get-location))))
                                    (send loco-obj set-previous-location! #f))))))
                                          
                            
                    (define/private (invert-reservations! loco-obj direction)
                      (let ((collected-segments (remq (send loco-obj get-location)(send loco-obj made-reservations?)))
                            (potential-path '()))
                        (for-each (λ (collected-segment)
                                    (when (dblock? (send railway get-track collected-segment))
                                      (let ((blue-flag #f))
                                        (let find-blue-flag ((candidates (get-neighbors (send railway get-railway-graph) collected-segment)))
                                          (unless (null? candidates)
                                            (if (eq? (send loco-obj get-loco-id)(send (send railway get-track (car candidates))reserved?))
                                                (set! blue-flag (car candidates))
                                                (find-blue-flag (cdr candidates)))))
                                        (define-values (distances parents)(dijkstra (send railway get-railway-graph)collected-segment))
                                        (for-each (λ (adjacent-dblock)
                                                    (let* ((path (path-to-source parents collected-segment adjacent-dblock))
                                                           (contains-blue-flag? (findf (λ (segment)
                                                                                         (eq? segment blue-flag))
                                                                                       path)))
                                                      (when contains-blue-flag?
                                                        (set! potential-path (append path potential-path)))))
                                                  (get-neighbors (send railway get-dblock-graph) collected-segment)))))
                                  collected-segments)
                        (set! potential-path (remove-duplicates potential-path))
                        (if (andmap (λ (segment)
                                      (let ((track-obj (send railway get-track segment)))
                                        (if (send track-obj reserved?)
                                            (eq? (send track-obj reserved?)(send loco-obj get-loco-id))
                                            (if (dblock? track-obj)
                                                (if (send loco-obj manual?)
                                                    (send loco-obj manual?)
                                                    (if (send track-obj occupied?)
                                                        (eq? (send track-obj occupied?)(send loco-obj get-loco-id))
                                                        #t))
                                                #t))))
                                    potential-path)
                            (begin
                              (cancel-reservations! (send loco-obj made-reservations?)) ;; unreserve current reservations
                              (reserve-tracks! (send loco-obj get-loco-id) potential-path)
                              (send (send railway get-track (send loco-obj get-location)) vacant!)
                              (send loco-obj reserve! potential-path)
                              (send-message out (list 'reserved-tracks (send loco-obj get-loco-id) (send loco-obj made-reservations?)))
                              (send-message out (list 'free (send loco-obj get-location)))
                              (send loco-obj set-location! #f)) ;;reserve new path, change direction, clear internal current 
                            (halt! loco-obj)))) ;; set speed 0.


                    (define/private (invert-direction! loco-obj)
                      (let ((direction (if (eq? (send loco-obj get-direction) 'forward)
                                           'backwards
                                           'forward)))
                        (set-loco-spd! (send loco-obj get-loco-id)(send loco-obj set-direction! direction))))
                    
                    (define/private (align-and-reserve! loco-obj path)
                      (for-each (λ (id)
                                  (let ((track-obj (send railway get-track id)))
                                    (send track-obj reserve! (send loco-obj get-loco-id))
                                    (when (switch? track-obj)
                                      (unless (findf (λ (track)
                                                       (eq? track (send track-obj get-linked-track)))
                                                     path)
                                        (if (= (send track-obj get-status)1)
                                            (begin
                                              (send track-obj set-status! 2)
                                              (set-switch-pos! (send track-obj get-track-id) 2)
                                              (send-message out (list 'set-switch-position! (send track-obj get-track-id) 2)))
                                            (begin
                                              (send track-obj set-status! 1)
                                              (set-switch-pos! (send track-obj get-track-id) 1)
                                              (send-message out (list 'set-switch-position! (send track-obj get-track-id) 1))))))))
                                path)
                      (displayln (list (send loco-obj get-loco-id)(car path)(list (length (get-neighbors (send railway get-railway-graph) (car path)))
                                                                                  (send loco-obj get-previous-location))))
                      (if (and (< (length (get-neighbors (send railway get-railway-graph) (car path)))2)
                               (send loco-obj get-previous-location))
                          (begin
                            (invert-previous! loco-obj)
                            (invert-direction! loco-obj)
                            (send-message out (list 'add-log! (string-append "Changed direction of "
                                                                             (symbol->string (send loco-obj get-loco-id))
                                                                             " to "
                                                                             (symbol->string (send loco-obj get-direction))))))
                          (when (send loco-obj get-previous-location)
                            (let ((previous-path (fewest-vertices-path (send railway get-railway-graph)
                                                                       (send loco-obj get-previous-location)
                                                                       (send loco-obj get-location))))
                              (when (ormap (λ (path-segment)
                                             (if (not (eq? (send loco-obj get-location) path-segment))
                                                 (findf (λ (id)
                                                          (eq? id path-segment))
                                                        previous-path)
                                                 #f))
                                           path)
                                (invert-previous! loco-obj)
                                (invert-direction! loco-obj)
                                (send-message out (list 'add-log! (string-append "Changed direction of "
                                                                                 (symbol->string (send loco-obj get-loco-id))
                                                                                 " to "
                                                                                 (symbol->string (send loco-obj get-direction)))))))))
                      (send-message out (list 'reserved-tracks (send loco-obj get-loco-id) path))
                      (send loco-obj reserve! path)
                      (send-message out (list 'set-route! (send loco-obj get-loco-id) path)))
                      
                      
                    (define/private (reserve-auto-path! loco-obj)
                      (let ((path (send loco-obj get-path)))
                        (unless (null? path)
                          (if (eq? (caar path)(send loco-obj get-location))
                              (if (check-if-free-for-loco (car path) loco-obj)
                                  (align-and-reserve! loco-obj (car path))
                                  (halt! loco-obj))
                              (begin
                                (send loco-obj set-path! (cdr path))
                                (reserve-auto-path! loco-obj))))))
                                     

                    (define/private (continue-path loco-obj)
                      (displayln (list (send loco-obj get-loco-id)(send loco-obj get-path)))
                      (if (eq? (send loco-obj get-location)
                               (send loco-obj get-destination))
                          (begin
                            (send loco-obj set-path! #f)
                            (send loco-obj set-destination! #f)
                            (send-message out (list 'reached-destination (send loco-obj get-loco-id)))
                            (set-speed-loco-obj! loco-obj 0))
                          (if (or (eq? (caar (send loco-obj get-path))(send loco-obj get-location))
                                  (eq? (last (car (send loco-obj get-path)))(send loco-obj get-location)))
                              (reserve-auto-path! loco-obj)
                              (send-message out (list 'recalculate-path (send loco-obj get-loco-id)
                                                      (send loco-obj get-destination))))))
                    
                    ;; updates the location of the trainq and the occupancy of a detection block.
                    (define/private (update-infrabel)
                      (send railway for-each-loco (λ (loco-id loco-obj)
                                                    (let  ((actual-location (get-loco-detection-block loco-id)))
                                                      (when actual-location
                                                        (let ((internal-location (send loco-obj get-location)))
                                                          (unless (eq? internal-location actual-location)
                                                            (when internal-location
                                                              (send-message out (list 'free! internal-location))
                                                              (send (send railway get-track internal-location) vacant!))
                                                            (send (send railway get-track actual-location) occupy! loco-id)
                                                            (send loco-obj set-location! actual-location)
                                                            (send-message out (list 'occupy! loco-id actual-location))
                                                            (cancel-reservations! (send loco-obj made-reservations?))
                                                            (send loco-obj clear-reservations!)
                                                            (if (send loco-obj get-destination)
                                                                (unless (zero? (send loco-obj get-speed))
                                                                  (continue-path loco-obj))
                                                                (make-reservations! loco-obj)))))))))

                            
                    ;; links switches with the correctly linked tracks of the simulator/hardware
                    (define/private (link-switches)
                      (send railway for-each-track (λ (track-id track-segment)
                                                     (when (switch? track-segment)
                                                       (let ((hw-status (get-switch-pos (send track-segment get-track-id))))
                                                         (send track-segment set-status! hw-status))))))

                    ;; adds the setup-hardware infrastructure to the railway 
                    (define/private (initialize-setup-hardware)
                      (add-dblock! '1-1 (list 'S-10 'S-28))
                      (add-dblock! '1-2 (list 'S-9 'S-27))
                      (add-dblock! '1-3 (list 'S-27 'U-2))
                      (add-dblock! '1-4 (list '1-5 'S-26))
                      (add-dblock! '1-5 (list '1-4 'S-20))
                      (add-dblock! '1-6 (list '1-7 'S-5))
                      (add-dblock! '1-7 (list '1-6 'S-28))
                      (add-dblock! '1-8 (list 'S-25))
                      (add-dblock! '2-1 (list 'S-1))
                      (add-dblock! '2-2 (list 'S-3))
                      (add-dblock! '2-3 (list 'S-12 'S-6))
                      (add-dblock! '2-4 (list 'S-20 'S-23))
                      (add-dblock! '2-5 (list 'S-8))
                      (add-dblock! '2-6 (list 'S-4))
                      (add-dblock! '2-7 (list 'S-4))
                      (add-dblock! '2-8 (list 'S-16))
                      ;; inits of switch%s
                      
                      (add-switch! 'S-1 (list 'U-6 '2-1 'S-25))
                      (add-switch! 'S-2 (list 'U-6 'S-7 'S-3))
                      (add-switch! 'S-3 (list 'S-2 '2-2 'S-8))
                      (add-switch! 'S-4 (list 'S-8 '2-6 '2-7))
                      (add-switch! 'S-5 (list 'S-6 '1-6 'S-7))
                      (add-switch! 'S-6 (list 'S-5 '2-3 'S-20))
                      (add-switch! 'S-7 (list 'S-5 'U-5 'S-2))
                      (add-switch! 'S-8 (list 'S-3 '2-5 'S-4))
                      (add-switch! 'S-9 (list '1-2 'U-3 'S-11))
                      (add-switch! 'S-10 (list 'S-11 '1-1 's-16))
                      (add-switch! 'S-11 (list 'S-12 'S-9 'S-10))
                      (add-switch! 'S-12 (list 'S-11 'U-4 '2-3))
                      (add-switch! 'S-16 (list 'U-7 'S-10 '2-8))
                      (add-switch! 'S-20 (list '2-4 '1-5 'S-6))
                      (add-switch! 'S-23 (list '2-4 'S-24 'U-4))
                      (add-switch! 'S-24 (list 'S-23 'U-3 'U-2))
                      (add-switch! 'S-25 (list '1-8 'U-5 'S-1))
                      (add-switch! 'S-26 (list '1-4 'U-1 'S-27))
                      (add-switch! 'S-27 (list 'S-26 '1-3 '1-2))
                      (add-switch! 'S-28 (list '1-1 '1-7 'U-1))
            
                      (add-track! 'U-1 (list 'S-28 'S-26))
                      (add-track! 'U-2 (list '1-3 'S-24))
                      (add-track! 'U-3 (list 'S-24 'S-9))
                      (add-track! 'U-4 (list 'S-23 'S-12))
                      (add-track! 'U-5 (list 'S-25 'S-7))
                      (add-track! 'U-6 (list 'S-1 'S-2))
                      (add-track! 'U-7 (list 'S-16))
                      (link-switches)

                      (add-vertex! (send railway get-dblock-graph) '1-1)
                      (add-vertex! (send railway get-dblock-graph) '1-2)
                      (add-vertex! (send railway get-dblock-graph) '1-3)
                      (add-vertex! (send railway get-dblock-graph) '1-4)
                      (add-vertex! (send railway get-dblock-graph) '1-5)
                      (add-vertex! (send railway get-dblock-graph) '1-6)
                      (add-vertex! (send railway get-dblock-graph) '1-7)
                      (add-vertex! (send railway get-dblock-graph) '1-8)
                      (add-vertex! (send railway get-dblock-graph) '2-1)
                      (add-vertex! (send railway get-dblock-graph) '2-2)
                      (add-vertex! (send railway get-dblock-graph) '2-3)
                      (add-vertex! (send railway get-dblock-graph) '2-4)
                      (add-vertex! (send railway get-dblock-graph) '2-5)
                      (add-vertex! (send railway get-dblock-graph) '2-6)
                      (add-vertex! (send railway get-dblock-graph) '2-7)
                      (add-vertex! (send railway get-dblock-graph) '2-8)
                      (add-edge! (send railway get-dblock-graph) '1-1 '2-3)
                      (add-edge! (send railway get-dblock-graph) '1-1 '1-7)
                      (add-edge! (send railway get-dblock-graph) '1-1 '1-4)
                      (add-edge! (send railway get-dblock-graph) '1-1 '2-4)
                      (add-edge! (send railway get-dblock-graph) '1-2 '1-4)
                      (add-edge! (send railway get-dblock-graph) '1-2 '2-3)
                      (add-edge! (send railway get-dblock-graph) '1-2 '2-4)
                      (add-edge! (send railway get-dblock-graph) '1-3 '2-4)
                      (add-edge! (send railway get-dblock-graph) '1-3 '1-4)
                      (add-edge! (send railway get-dblock-graph) '1-4 '1-5)
                      (add-edge! (send railway get-dblock-graph) '1-4 '1-1)
                      (add-edge! (send railway get-dblock-graph) '1-4 '1-2)
                      (add-edge! (send railway get-dblock-graph) '1-4 '1-3)
                      (add-edge! (send railway get-dblock-graph) '1-5 '1-4)
                      (add-edge! (send railway get-dblock-graph) '1-5 '2-4)
                      (add-edge! (send railway get-dblock-graph) '1-6 '1-7)
                      (add-edge! (send railway get-dblock-graph) '1-6 '2-3)
                      (add-edge! (send railway get-dblock-graph) '1-6 '2-4)
                      (add-edge! (send railway get-dblock-graph) '1-7 '1-6)
                      (add-edge! (send railway get-dblock-graph) '1-7 '1-1)
                      (add-edge! (send railway get-dblock-graph) '1-8 '2-2)
                      (add-edge! (send railway get-dblock-graph) '1-8 '2-4)
                      (add-edge! (send railway get-dblock-graph) '1-8 '2-3)
                      (add-edge! (send railway get-dblock-graph) '1-8 '2-5)
                      (add-edge! (send railway get-dblock-graph) '1-8 '2-6)
                      (add-edge! (send railway get-dblock-graph) '1-8 '2-7)
                      (add-edge! (send railway get-dblock-graph) '2-1 '2-7)
                      (add-edge! (send railway get-dblock-graph) '2-1 '2-6)
                      (add-edge! (send railway get-dblock-graph) '2-1 '2-5)
                      (add-edge! (send railway get-dblock-graph) '2-1 '2-2)
                      (add-edge! (send railway get-dblock-graph) '2-1 '2-3)
                      (add-edge! (send railway get-dblock-graph) '2-1 '2-4))
                    ;; just in case if a switch isn't linked correctly

                    ;; adds the setup-straight infrastructure to the railway 
                    (define/private (initialize-setup-straight)
                      (add-dblock! 'D1 (list 'D2))
                      (add-dblock! 'D2 (list 'D1 'T1))
                      (add-dblock! 'D3 (list 'T1 'D4))
                      (add-dblock! 'D4 (list 'D3))
                      
                      (add-track! 'T1 (list 'D2 'D3))

                      (add-vertex! (send railway get-dblock-graph) 'D1)
                      (add-vertex! (send railway get-dblock-graph) 'D2)
                      (add-vertex! (send railway get-dblock-graph) 'D3)
                      (add-vertex! (send railway get-dblock-graph) 'D4)
                      (add-edge! (send railway get-dblock-graph) 'D1 'D2)
                      (add-edge! (send railway get-dblock-graph) 'D2 'D3)
                      (add-edge! (send railway get-dblock-graph) 'D3 'D4))

                    ;; adds the setup-straight-with-switch infrastructure to the railway 
                    (define/private (initialize-setup-straight-with-switch)
                      (add-dblock! 'D1 (list 'S1))
                      (add-dblock! 'D4 (list 'S1 'D5))
                      (add-dblock! 'D5 (list 'D4))
                      (add-dblock! 'D6 (list 'S1 'D7))
                      (add-dblock! 'D7 (list 'D6))

                      (add-switch! 'S1 (list 'D1 'D4 'D6))
                      (link-switches)
                      
                      (add-vertex! (send railway get-dblock-graph) 'D1)
                      (add-vertex! (send railway get-dblock-graph) 'D4)
                      (add-vertex! (send railway get-dblock-graph) 'D5)
                      (add-vertex! (send railway get-dblock-graph) 'D6)
                      (add-vertex! (send railway get-dblock-graph) 'D7)
                      (add-edge! (send railway get-dblock-graph) 'D1 'D4)
                      (add-edge! (send railway get-dblock-graph) 'D1 'D6)
                      (add-edge! (send railway get-dblock-graph) 'D7 'D6)
                      (add-edge! (send railway get-dblock-graph) 'D5 'D4))
                      
                    ;; adds the setup-loop infrastructure to the railway 
                    (define/private (initialize-setup-loop)
                      (add-dblock! 'D1 (list 'T1 'D2))
                      (add-dblock! 'D2 (list 'D3 'D1))
                      (add-dblock! 'D3 (list 'D2 'D4))
                      (add-dblock! 'D4 (list 'D5 'D3))
                      (add-dblock! 'D5 (list 'D4 'T2))
                      (add-dblock! 'D6 (list 'T2 'D7))
                      (add-dblock! 'D7 (list 'D6 'D8))
                      (add-dblock! 'D8 (list 'D7 'T1))
                      
                      (add-track! 'T1 (list 'D1 'D8))
                      (add-track! 'T2 (list 'D5 'D6))

                      (add-vertex! (send railway get-dblock-graph) 'D1)
                      (add-vertex! (send railway get-dblock-graph) 'D2)
                      (add-vertex! (send railway get-dblock-graph) 'D3)
                      (add-vertex! (send railway get-dblock-graph) 'D4)
                      (add-vertex! (send railway get-dblock-graph) 'D6)
                      (add-vertex! (send railway get-dblock-graph) 'D7)
                      (add-vertex! (send railway get-dblock-graph) 'D8)
                      (add-edge! (send railway get-dblock-graph) 'D1 'D2)
                      (add-edge! (send railway get-dblock-graph) 'D1 'D8)
                      (add-edge! (send railway get-dblock-graph) 'D5 'D6)
                      (add-edge! (send railway get-dblock-graph) 'D2 'D3)
                      (add-edge! (send railway get-dblock-graph) 'D3 'D4)
                      (add-edge! (send railway get-dblock-graph) 'D4 'D5)
                      (add-edge! (send railway get-dblock-graph) 'D7 'D6)
                      (add-edge! (send railway get-dblock-graph) 'D7 'D8))
                    
                    ;; adds the setup-loop-and-switches infrastructure to the railway 
                    (define/private (initialize-setup-loop-and-switches)
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
                      (link-switches)

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
                      (add-edge! (send railway get-dblock-graph) 'D8 'D9))
                    
                    (define/private (initialize-infrabel . setup-type)
                      (if hardware-active
                          (begin
                            (initialize-setup-hardware)
                            (start))
                          (if (not (null? setup-type))
                              (case (car setup-type)
                                [(setup-hardware)(setup-hardware)
                                                 (initialize-setup-hardware)
                                                 (start)]
                                [(setup-straight)(setup-straight)
                                                 (initialize-setup-straight)
                                                 (start)]
                                [(setup-straight-with-switch)(setup-straight-with-switch)
                                                             (initialize-setup-straight-with-switch)
                                                             (start)]
                                [(setup-loop)(setup-loop)
                                             (initialize-setup-loop)
                                             (start)]
                                [(setup-loop-and-switches)(setup-loop-and-switches)
                                                          (initialize-setup-loop-and-switches)
                                                          (start)]
                                [else (error "Given invalid setup type: " setup-type)]) 
                              (error  "Given invalid setup type: " setup-type))))

                    ;--------------------- public methods

                    ;; adds a locomotive to the simulator/hardware and to the internal railway
                    (define/public (add-loco! loco-id prev-id current-id)
                      (when (and (dblock? (send railway get-track prev-id))
                                 (dblock? (send railway get-track current-id)))
                        (let ((locomotive (send railway make-locomotive loco-id 'forward current-id prev-id)))
                          (when locomotive
                            (send railway add-locomotive! locomotive)
                            (add-loco loco-id prev-id current-id)
                            (send (send railway get-track current-id) occupy! loco-id)
                            (send-message out (list 'occupy! loco-id current-id))
                            (send-message out (list 'add-log! (string-append "Added locomotive with id: "
                                                                             (symbol->string loco-id)
                                                                             " at location "
                                                                             (symbol->string current-id))))))))

                    ;; removes a locomotive
                    (define/public (remove-loco! loco-id)
                      (unless hardware-active
                        (let ((loco-obj (send railway get-locomotive loco-id)))
                          (when loco-obj
                            (cancel-reservations! (send loco-obj made-reservations?))
                            (send railway remove-locomotive! loco-id)
                            (remove-loco loco-id)
                            (send-message out (list 'add-log! (string-append "Removed locomotive with id: "
                                                                             (symbol->string loco-id))))))))
                    
                    ;;changes a locomotives' speed
                    (define/public (set-loco-speed! loco-id new-speed)
                      (let ((loco-obj (send railway get-locomotive loco-id)))
                        (when loco-obj
                          (if (send loco-obj get-destination)
                              (cond
                                ((zero? new-speed)
                                 (unless (zero? (send loco-obj get-speed))
                                   (if (get-loco-detection-block loco-id)
                                       (begin
                                         (cancel-reservations! (send loco-obj made-reservations?))
                                         (set-speed-loco-obj! (send railway get-locomotive loco-id) new-speed)
                                         (send-message out (list 'add-log! "Set speed of "
                                                                 (symbol->string loco-id)
                                                                 " to "
                                                                 (number->string new-speed))))
                                       (begin
                                         (set-speed-loco-obj! (send railway get-locomotive loco-id) new-speed)
                                         (send-message out (list 'add-log! "Set speed of "
                                                                 (symbol->string loco-id)
                                                                 " to "
                                                                 (number->string new-speed)))))))
                                ((zero? (send loco-obj get-speed))
                                 (unless (zero? new-speed)
                                   (if (get-loco-detection-block loco-id)
                                       (begin
                                         (continue-path (send railway get-locomotive loco-id))
                                         (when (send loco-obj made-reservations?)
                                           (set-speed-loco-obj! (send railway get-locomotive loco-id) new-speed)
                                           (send-message out (list 'add-log! (string-append "Set speed of "
                                                                                            (symbol->string loco-id)
                                                                                            " to "
                                                                                            (number->string new-speed))))))
                                       (begin
                                         (set-speed-loco-obj! (send railway get-locomotive loco-id) new-speed)
                                         (send-message out (list 'add-log! (string-append "Set speed of "
                                                                                          (symbol->string loco-id)
                                                                                          " to "
                                                                                          (number->string new-speed))))))))
                                (else (begin
                                        (set-speed-loco-obj! (send railway get-locomotive loco-id) new-speed)
                                        (send-message out (list 'add-log! (string-append "Set speed of "
                                                                                         (symbol->string loco-id)
                                                                                         " to "
                                                                                         (number->string new-speed)))))))
                              (cond
                                ((zero? (send loco-obj get-speed))
                                 (if (get-loco-detection-block loco-id)
                                     (unless (zero? new-speed)
                                       (make-reservations! (send railway get-locomotive loco-id))
                                       (when (send (send railway get-locomotive loco-id)made-reservations?)
                                         (set-speed-loco-obj! (send railway get-locomotive loco-id) new-speed)
                                         (send-message out (list 'add-log! (string-append "Set speed of "
                                                                                          (symbol->string loco-id)
                                                                                          " to "
                                                                                          (number->string new-speed))))))
                                     (unless (zero? new-speed)
                                       (when (send (send railway get-locomotive loco-id)made-reservations?)
                                         (set-speed-loco-obj! (send railway get-locomotive loco-id)new-speed)
                                         (send-message out (list 'add-log! (string-append "Set speed of "
                                                                                          (symbol->string loco-id)
                                                                                          " to "
                                                                                          (number->string new-speed))))))))
                                ((zero? new-speed)
                                 (if (get-loco-detection-block loco-id)
                                     (unless (zero? (send loco-obj get-speed))
                                       (cancel-reservations! (send loco-obj made-reservations?))
                                       (set-speed-loco-obj! (send railway get-locomotive loco-id) new-speed)
                                       (send-message out (list 'add-log! (string-append "Set speed of "
                                                                                        (symbol->string loco-id)
                                                                                        " to "
                                                                                        (number->string new-speed)))))
                                     (unless (zero? (send loco-obj get-speed))
                                       (set-speed-loco-obj! (send railway get-locomotive loco-id) new-speed)
                                       (send-message out (list 'add-log! (string-append "Set speed of "
                                                                                        (symbol->string loco-id)
                                                                                        " to "
                                                                                        (number->string new-speed)))))))
                                (else (begin
                                        (set-speed-loco-obj! (send railway get-locomotive loco-id) new-speed)
                                        (send-message out (list 'add-log! (string-append "Set speed of "
                                                                                         (symbol->string loco-id)
                                                                                         " to "
                                                                                         (number->string new-speed)))))))))))
                    
                    
                    
                    ;; gets the locomotive speed
                    (define/public (get-loco-speed loco-id)
                      (send (send railway get-locomotive loco-id)get-speed))
                    
                    ;; sets the locomotive destination with given path
                    (define/public (set-loco-destination! loco-id destination-id path)
                      (send (send railway get-locomotive loco-id) set-destination! destination-id)
                      (send (send railway get-locomotive loco-id) set-path! path)
                      (unless (and destination-id path)
                        (send-message out (list 'set-route! loco-id "No destination set so no route calculated")))
                      (unless (send (send railway get-locomotive loco-id)made-reservations?)
                        (send (send railway get-locomotive loco-id) reserve! '()))
                      (when (and (get-loco-detection-block loco-id)destination-id path)
                        (when (send (send railway get-locomotive loco-id) get-previous-location)
                          (define-values (distance values)(dijkstra (send railway get-railway-graph)(send (send railway get-locomotive loco-id) get-location)))
                          (let* ((loco-obj (send railway get-locomotive loco-id))
                                 (previous-path (path-to-source values (send loco-obj get-location)(send loco-obj get-previous-location))))
                            (when (ormap (λ (track-id)
                                           (if (not (eq? track-id (send loco-obj get-location)))
                                               (findf (λ (id)
                                                        (eq? track-id id))
                                                      previous-path)
                                               #f))
                                         (car path))
                              (invert-previous! loco-obj)
                              (invert-direction! loco-obj))))
                        (send (send railway get-locomotive loco-id)set-location! #f)))

                    ;; sets the locomotive direction
                    (define/public (set-loco-direction! loco-id direction)
                      (let ((loco (send railway get-locomotive loco-id)))
                        (unless (send loco get-destination)
                          (define (changing-direction? new-d)
                            (or (and (eq? direction 'forward)(eq? (send loco get-direction) 'backwards))
                                (and (eq? direction 'backwards)(eq? (send loco get-direction) 'forward))))
                          (when (changing-direction? direction)
                            (if (get-loco-detection-block loco-id)
                                (if (zero? (send loco get-speed))
                                    (begin
                                      (invert-previous! (send railway get-locomotive loco-id))
                                      (set-loco-spd! loco-id (send (send railway get-locomotive loco-id) set-direction! direction))
                                      (send-message out (list 'add-log! (string-append "Changed direction of "
                                                                                       (symbol->string loco-id)
                                                                                       " to "
                                                                                       (symbol->string direction)))))
                                    (begin
                                      (invert-previous! (send railway get-locomotive loco-id))
                                      (cancel-reservations! (send (send railway get-locomotive loco-id)made-reservations?))
                                      (make-reservations! (send railway get-locomotive loco-id))
                                      (set-loco-spd! loco-id (send (send railway get-locomotive loco-id) set-direction! direction))
                                      (send-message out (list 'add-log! (string-append "Changed direction of "
                                                                                       (symbol->string loco-id)
                                                                                       " to "
                                                                                       (symbol->string direction))))))
                                (begin
                                  (invert-previous! (send railway get-locomotive loco-id))
                                  (invert-reservations! (send railway get-locomotive loco-id) direction)
                                  (set-loco-spd! loco-id (send (send railway get-locomotive loco-id) set-direction! direction))
                                  (send-message out (list 'add-log! (string-append "Changed direction of "
                                                                                   (symbol->string loco-id)
                                                                                   " to "
                                                                                   (symbol->string direction))))))))))
                        
                                      
                               
                          
                    ;; returns a list of all the ids of the blocks
                    (define/public (get-list-of-dblocks)
                      (let ((res  '()))
                        (send railway for-each-track (λ (id obj)
                                                       (when (dblock? obj)
                                                         (set! res (cons (list id
                                                                               (send obj reserved?)
                                                                               (send obj occupied?))
                                                                         res)))))
                        res))

                    ;; returns a list of all the ids of the switches
                    (define/public (get-list-of-switches)
                      (let ((res '()))
                        (send railway for-each-track (λ (id obj)
                                                       (when (switch? obj)
                                                         (set! res (cons (list id
                                                                               (send obj reserved?)
                                                                               (send obj get-status))
                                                                         res)))))
                        res))

                    ;; returns a list of all the ids of the locomotives
                    (define/public (get-list-of-locos)
                      (let ((res '()))
                        (send railway for-each-loco (λ (id obj)
                                                      (set! res (cons (list->vector
                                                                       (list id
                                                                             (send obj manual?)
                                                                             (send obj get-speed)
                                                                             (send obj get-direction)
                                                                             (send obj made-reservations?)
                                                                             (send obj get-destination)
                                                                             (send obj get-path)))                                                        
                                                                      res))))
                        res))

                   
                    ;; sets a switch position to the given value
                    (define/public (set-switch-position! switch-id new-position)
                      (let* ((switch (send railway get-track switch-id))
                             (reserved-by (send switch reserved?)))
                        (if reserved-by
                            (unless (send (send railway get-locomotive reserved-by) get-destination)
                              (send (send railway get-track switch-id) set-status! new-position)
                              (set-switch-pos! switch-id new-position))
                            (begin
                              (send (send railway get-track switch-id) set-status! new-position)
                              (set-switch-pos! switch-id new-position)))))

                    ;; sets manual mode or disables
                    (define/public (set-manual-mode! loco-id boolean)
                      (send (send railway get-locomotive loco-id) manual-override boolean))

                    ;; returns the railway within infrabel 
                    (define/public (fetch-railway)
                      (get-edges (send railway get-railway-graph)))

                    ;; returns the reservation graph 
                    (define/public (fetch-reservation-graph)
                      (get-edges (send railway get-dblock-graph)))

                    ;;;;;;;;;;;;;;;;;;;;;;;;
                    ;;     TCP handler    ;;
                    ;;;;;;;;;;;;;;;;;;;;;;;;
                    
                    ;; handles incoming and optionally returns messages to client
                    (define/private (tcp-handler)
                      (unless (port-closed? in)
                        (when (char-ready? in)
                          (let ((msg (read in)))
                            (displayln msg)
                            (if (eof-object? msg)
                                (tcp-close server)
                                (match msg
                                  [(list 'hardware?)(send-message out (list hardware-active))]
                                  [(list 'get-list-of-switches)(send-message out (get-list-of-switches))]
                                  [(list 'get-list-of-dblocks)(send-message out (get-list-of-dblocks))]
                                  [(list 'get-list-of-locos)(send-message out (get-list-of-locos))]
                                  [(list 'get-reservation-graph)(send-message out(fetch-reservation-graph))]
                                  [(list 'get-railway-graph)(send-message out (fetch-railway))]
                                  [(list 'set-switch-position! _ _)(set-switch-position! (second msg)(third msg))]
                                  [(list 'set-loco-destination! _ _ _)(set-loco-destination! (second msg)(third msg)(fourth msg))]
                                  [(list 'set-loco-direction! _ _)(set-loco-direction! (second msg)(third msg))]
                                  [(list 'set-loco-speed! _ _)(set-loco-speed! (second msg)(third msg))]
                                  [(list 'remove-loco! _)(remove-loco! (second msg))]
                                  [(list 'add-loco! _ _ _)(add-loco! (second msg)(third msg)(fourth msg))]
                                  [(list 'get-loco-location _)(send-message out (list (send (send railway get-locomotive (second msg))get-location)))]
                                  [(list 'set-manual-mode! _ _)(set-manual-mode! (second msg)(third msg))]
                                  [(list 'initialize-infrabel _)(initialize-infrabel (cadr msg))]
                                  [(list 'close-connection)(tcp-close server)
                                                           (close-output-port out)
                                                           (close-input-port in)]
                                  [_ (display  "incorrect message syntax: " )
                                     (displayln msg)]))))))

                    ;; starts the main loop and continues calling it
                    (define (main-loop)
                      (update-infrabel)
                      (tcp-handler)
                      (main-loop))


                    
                    (define/public (start-infrabel)
                      (thread main-loop))))

(define (infrabel? object)
  (is-a? object infrabel%))
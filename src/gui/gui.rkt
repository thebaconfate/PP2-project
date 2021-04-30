#lang racket

(require racket/gui/base)
(require "../other/other-procedures.rkt")


(provide gui%)
(define gui% (class object%
               (init-field frame-width frame-height main-spacing)
               (super-new)
               (define frames (make-hash))
               (define panels (make-hash))
               (define dialogs (make-hash))
               (define buttons (make-hash))
               (define messages (make-hash))
               (define textboxes (make-hash))
               (define loco-draw-data (make-hash))
               (define switch-draw-data (make-hash))
               (define dblock-draw-data (make-hash))
               (define margin 5)
               (define loco-text-margin 14)
               (define loco-slider-margin 2)
               (define loco-choice-margin (- loco-text-margin 3))
               
               (define log-counter 0)

               ;; a few abstractions
               (define/private (get-frame id)
                 (hash-ref frames id #f))
               (define/private (set-frame! id frame)
                 (hash-set! frames id frame))

               (define/private (get-panel id)
                 (hash-ref panels id #f))
               (define/private (set-panel! id panel)
                 (hash-set! panels id panel))
               
               (define/private (get-dialog id)
                 (hash-ref dialogs id #f))
               (define/private (set-dialog! id dialog)
                 (hash-set! dialogs id dialog))
               
               (define/private (get-button id)
                 (hash-ref buttons id #f))
               (define/private (set-button! id button)
                 (hash-set! buttons id button))

               (define/private (get-message id)
                 (hash-ref messages id #f))
               (define/private (set-message! id message)
                 (hash-set! messages id message))

               (define/private (get-textbox id)
                 (hash-ref textboxes id #f))
               (define/private (set-textbox! id textbox)
                 (hash-set! textboxes id textbox))

               (define/private (get-loco-data id)
                 (hash-ref loco-draw-data id #f))
               (define/private (set-loco-data! id vct)
                 (hash-set! loco-draw-data id vct))

               (define/private (get-switch-data id)
                 (hash-ref switch-draw-data id #f))
               (define/private (set-switch-data! id vct)
                 (hash-set! switch-draw-data id vct))

               (define/private (get-dblock-data id)
                 (hash-ref dblock-draw-data id #f))
               (define/private (set-dblock-data! id vct)
                 (hash-set! dblock-draw-data id vct))

               (define (id-msg vct)
                 (vector-ref vct 0))
               (define (speed-slider vct)
                 (vector-ref vct 1))
               (define (direction vct)
                 (vector-ref vct 2))
               (define (manual vct)
                 (vector-ref vct 3))
               (define (location vct)
                 (vector-ref vct 4))
               (define (destination vct)
                 (vector-ref vct 5))
               (define (id-msg2 vct)
                 (vector-ref vct 6))
               (define (route vct)
                 (vector-ref vct 7))

               (define (position vct)
                 (vector-ref vct 1))

               (define (occupancy vct)
                 (vector-ref vct 1))
               (define (reservation vct)
                 (vector-ref vct 2))

               (define number-of-entries 20)
               (define log# '())
               (define log-entries '())
                 
                                                             
               (define/private (init-log)
                 (let loop ((idx 1))
                   (when (<= idx number-of-entries)
                     (set! log# (cons (new message%
                                           [parent (get-panel 'log-entry-number)]
                                           [label (string-append (number->string idx)".")])
                                      log#))
                     (loop (+ idx 1))))
                 (let loop ((idx 1))
                   (when (<= idx number-of-entries)
                     (set! log-entries (cons (new message%
                                           [parent (get-panel 'log-entry)]
                                           [label "No log entry, please interact with the interface to populate the log"])
                                      log-entries))
                     (loop (+ idx 1)))))
                 
               (define/public (add-log! log)
                 (for-each (lambda (msg)
                             (send (get-panel 'log-entry) delete-child msg))
                           log-entries)
                 (set! log-entries (cons (new message%
                                              [parent (get-panel 'log-entry)]
                                              [label log])
                                         log-entries))
                 (set! log-entries (drop-right log-entries 1))
                 (for-each (lambda (msg)
                             (send (get-panel 'log-entry) add-child msg))
                           (cdr log-entries)))
                           


               (define/private (setup-main-tab)
                 (set-panel! 'loco-id-column (new vertical-panel%
                                                  [parent (get-panel "Main")]
                                                  [alignment '(center top)]))
               
                 (set-panel! 'loco-speed-column (new vertical-panel%
                                                     [parent (get-panel "Main")]
                                                     [alignment '(center top)]))
                                  
                 (set-panel! 'loco-direction-column (new vertical-panel%
                                                         [parent (get-panel "Main")]
                                                         [alignment '(center top)]))
                 
                 (set-panel! 'loco-manual-column (new vertical-panel%
                                                      [parent (get-panel "Main")]
                                                      [alignment '(center top)]))
                 
                 (set-panel! 'loco-location-column (new vertical-panel%
                                                        [parent (get-panel "Main")]
                                                        [alignment '(center top)]))
                 
                 (set-panel! 'loco-destination-column (new vertical-panel%
                                                           [parent (get-panel "Main")]
                                                           [alignment '(center top)]))
                 
                 (set-panel! 'switch-id-column (new vertical-panel%
                                                    [parent (get-panel "Main")]
                                                    [alignment '(center top)]))
                 
                 (set-panel! 'switch-position-column (new vertical-panel%
                                                          [parent (get-panel "Main")]
                                                          [alignment '(center top)]))
                 (set-panel! 'switch-reservation-column (new vertical-panel%
                                                          [parent (get-panel "Main")]
                                                          [alignment '(center top)]))
                 
                 (set-panel! 'dblock-id-column (new vertical-panel%
                                                    [parent (get-panel "Main")]
                                                    [alignment '(center top)]))
                 
                 (set-panel! 'dblock-occupancy-column (new vertical-panel%
                                                           [parent (get-panel "Main")]
                                                           [alignment '(center top)]))
                
                 (set-panel! 'dblock-reservation-column (new vertical-panel%
                                                             [parent (get-panel "Main")]
                                                             [alignment '(center top)]))
                 (set-message! 'loco-info-header-id (new message%
                                                         [parent (get-panel 'loco-id-column)]
                                                         [label "Loco-ID"]))
                
                 (set-message! 'loco-info-header-speed (new message%
                                                            [parent (get-panel 'loco-speed-column)]
                                                            [label "Speed"]))

                 (set-message! 'loco-info-header-direction (new message%
                                                                [parent (get-panel 'loco-direction-column)]
                                                                [label "Direction"]))

                 (set-message! 'loco-info-header-manual (new message%
                                                                [parent (get-panel 'loco-manual-column)]
                                                                [label "Manual"]))
               
                 (set-message! 'loco-info-header-location (new message%
                                                               [parent (get-panel 'loco-location-column)]
                                                               [label "Location"]))
                
                 (set-message! 'loco-info-header-destination (new message%
                                                                  [parent (get-panel 'loco-destination-column)]
                                                                  [label "Destination"]))
                
                 (set-message! 'switch-info-header (new message%
                                                        [parent (get-panel 'switch-id-column)]
                                                        [label "Switch-ID"]))
                
                 (set-message! 'switch-position-header (new message%
                                                         [parent (get-panel 'switch-position-column)]
                                                         [label "Position"]))
                 
                 (set-message! 'switch-reservations-header (new  message%
                                                                 [parent (get-panel 'switch-reservation-column)]
                                                                 [label "Reservation"]))
                
                 (set-message! 'dblock-info-header (new message%
                                                        [parent (get-panel 'dblock-id-column)]
                                                        [label "Dblock-ID"]))

                 (set-message! 'dblock-info-status (new message%
                                                        [parent (get-panel 'dblock-occupancy-column)]
                                                        [label "Occupancy"]))
                 
                 (set-message! 'dblock-info-reservations (new message%
                                                              [parent (get-panel 'dblock-reservation-column)]
                                                              [label "Reservation"])))

               (define/private (setup-adv-instruc-tab)
                 (set-panel! 'add/remove-panel (new vertical-panel%
                                                    [parent (get-panel "Adv. Instructions")]
                                                    [style '(border)]
                                                    [alignment '(center top)]))
                 
                 (set-panel! 'add-title-panel (new horizontal-panel%
                                                   [parent (get-panel 'add/remove-panel)]
                                                   [alignment '(center center)]))
                 
                 (set-panel! 'add-panel (new horizontal-panel%
                                             [parent (get-panel 'add/remove-panel)]
                                             [alignment '(center center)]))
                 
                 (set-message! 'add-msg (new message%
                                            [parent (get-panel 'add-title-panel)]
                                            [label "To add locomotive insert locomotive ID, previous segment and current segment."]
                                            [min-width main-spacing]))
                 
                 (set-panel! 'remove-title-panel (new horizontal-panel%
                                                 [parent (get-panel 'add/remove-panel)]
                                                 [alignment '(center center)]))

                 (set-panel! 'remove-panel (new horizontal-panel%
                                                [parent (get-panel 'add/remove-panel)]
                                                [alignment '(center center)]))

                 (set-message! 'remove-title (new message%
                                                  [parent (get-panel 'remove-title-panel)]
                                                  [label "To remove locomotive insert locomotive ID and press remove."]
                                                  [min-width main-spacing]))
               
               
                 (set-panel! 'calc-route-panel (new horizontal-panel%
                                                    [parent (get-panel "Adv. Instructions")]
                                                    [alignment '(center top)]))
                 
                 (set-panel! 'vert-calc-r-panel (new vertical-panel%
                                                     [parent (get-panel 'calc-route-panel)]
                                                     [alignment '(center top)]))

                 (set-panel! 'instruc-panel (new horizontal-panel%
                                                 [parent (get-panel 'vert-calc-r-panel)]
                                                 [alignment '(center top)]))
                 
                 (set-panel! 'instruc-id-panel (new vertical-panel%
                                                    [parent (get-panel 'instruc-panel)]
                                                    [style '(border)]
                                                    [alignment '(center top)]))
                 
                 (set-message! 'ins-id-title (new message%
                                                  [parent (get-panel 'instruc-id-panel)]
                                                  [label "Loco-ID"]))
                 
                 (set-panel! 'route-panel (new vertical-panel%
                                               [parent (get-panel 'calc-route-panel)]
                                               [style '(border)]
                                               [alignment '(center top)]))
                 
                 (set-message! 'route-title (new message%
                                                 [parent (get-panel 'route-panel)]
                                                 [label "Route"])))
               
               (define/private (init-gui)
                 ;; initiates the main frame
                 (set-frame! 'main-frame (new frame%
                                              [label "NMBS"]
                                              [width frame-width]
                                              [height frame-height]))
                 ;; initiates the tabs
                 (set-panel! 'tab-panel (new tab-panel%
                                             [parent (get-frame 'main-frame)]
                                             [alignment '(left top)]
                                             [choices (list "Menu" "Main" "Adv. Instructions" "Log")]
                                             [callback (lambda (panel event)
                                                         (send panel change-children (lambda (children)
                                                                                       (list (get-panel
                                                                                              (send panel get-item-label
                                                                                                    (send panel get-selection)))))))]))
                 ;;;;;;;;;;;;
                 ;;  Menu  ;;
                 ;;;;;;;;;;;;
                 
                 ;; the frame for the menu tab
                 (set-panel! "Menu" (new vertical-panel%
                                         [parent (get-panel 'tab-panel)]
                                         [style '(border)]
                                         [alignment '(center center)]))
                 ;; header for the menu in menu tab
                 (set-message! 'menu-tab-header (new message%
                                                     [parent (get-panel "Menu")]
                                                     [label "Menu"]))

                 ;;;;;;;;;;;;
                 ;;  Main  ;;
                 ;;;;;;;;;;;;
                 
                 ;; panel for the main tab
                 (set-panel! "Main" (new horizontal-panel%
                                         [parent (get-panel 'tab-panel)]
                                         [style '(border)]
                                         [alignment '(center top)]))

                 (setup-main-tab)

                 ;;;;;;;;;;;;;;;;;;;;
                 ;;  Adv. Instruc. ;;
                 ;;;;;;;;;;;;;;;;;;;;
                 (set-panel! "Adv. Instructions" (new vertical-panel%
                                                      [parent (get-panel 'tab-panel)]
                                                      [style '(border)]))
                 (setup-adv-instruc-tab)



                 ;;;;;;;;;;
                 ;;  Log ;;
                 ;;;;;;;;;;
                 (set-panel! "Log" (new vertical-panel%
                                        [parent (get-panel 'tab-panel)]
                                        [style '(border)]
                                        [alignment '(center top)]))
                 
                 (set-message! 'log-header (new message%
                                                [parent (get-panel "Log")]
                                                [label "NMBS LOG"]))
                 
                 (set-panel! 'log-entries (new horizontal-panel%
                                               [parent (get-panel "Log")]
                                               [style '(border)]
                                               [alignment '(left top)]))
                 
                 (set-panel! 'log-entry-number (new vertical-panel%
                                                    [parent (get-panel 'log-entries)]
                                                    [style '(border)]
                                                    [alignment '(left top)]))
                 
                 (set-panel! 'log-entry (new vertical-panel%
                                             [parent (get-panel 'log-entries)]
                                             [style '(border)]
                                             [alignment '(left top)]))
                 (init-log))


               (define/public (set-loco-speed! id spd)
                 (send (speed-slider (get-loco-data id)) set-value spd))

               (define/public (set-dblock-occupied! dblock-id loco-id)
                 (send (location (get-loco-data loco-id))set-label (symbol->string dblock-id))
                 (send (occupancy (get-dblock-data dblock-id)) set-label (symbol->string loco-id)))

               (define/public (free! dblock)
                 (send (occupancy (get-dblock-data dblock)) set-label "Vacant"))

               (define/public (reserve-dblock! dblock loco)
                 (send (reservation (get-dblock-data dblock)) set-label (symbol->string loco)))

               (define/public (reserve-switch! switch loco)
                 (send (reservation (get-switch-data switch)) set-label (symbol->string loco)))

               (define/public (free-dblock! dblock)
                 (send (reservation (get-dblock-data dblock)) set-label "Vacant"))

               (define/public (free-switch! switch)
                 (send (reservation (get-switch-data switch)) set-label "Vacant"))

               (define/public (set-switch-position! switch-id pos)
                 (send (position (get-switch-data switch-id)) set-selection pos))

               (define/public (halt! loco-id)
                 (set-loco-speed! loco-id 0))
               
               (define/public (set-to-not-set loco-id)
                 (send (destination(get-loco-data loco-id)) set-selection 0)
                 (halt! loco-id)
                 (send (route (get-loco-data loco-id)) set-label "destination reached"))

               (define/public (set-route! loco-id new-route)
                 (send (route (get-loco-data loco-id)) set-label (if (string? new-route)
                                                                     new-route
                                                                     (string-join (map (lambda (track-id)
                                                                                     (symbol->string track-id))
                                                                                   new-route)))))


               (define/public (add-locomotive! id spd spd-proc dir dir-proc manual-mode manual-proc loc dest dest-proc)
                 (let ((loco-info (make-vector 8)))
                   (vector-set! loco-info 0 (new message%
                                               [label (symbol->string id)]
                                               [parent  (get-panel 'loco-id-column)]
                                               [vert-margin loco-text-margin]))
                   (vector-set! loco-info 1 (new slider%
                                               [label #f]
                                               [min-value 0]	 
                                               [max-value 200]	 
                                               [parent (get-panel 'loco-speed-column)]	 
                                               [callback (lambda (button e)(spd-proc (send button get-value)))]	 
                                               [init-value spd]
                                               [vert-margin loco-slider-margin]))
                   (vector-set! loco-info 2 (new choice%
                                               [label #f]	 
                                               [choices (list "Forward" "Backwards")]	 
                                               [parent (get-panel 'loco-direction-column)]	 
                                               [callback (lambda(choice ev)
                                                           (dir-proc (send (direction(get-loco-data id))get-selection)))]	 
                                               [selection (if (eq? dir 'forward)
                                                              0
                                                              1)]
                                               [vert-margin loco-choice-margin]))
                   (vector-set! loco-info 3 (new choice%
                                               [label #f]	 
                                               [choices (list "No" "Yes")]	 
                                               [parent (get-panel 'loco-manual-column)]	 
                                               [callback (lambda(choice ev)
                                                           (manual-proc (send (manual(get-loco-data id))get-selection)))]	 
                                               [selection (if manual-mode
                                                              1
                                                              0)]
                                               [vert-margin loco-choice-margin]))
                   (vector-set! loco-info 4 (new message%
                                               [label (symbol->string loc)]
                                               [parent (get-panel 'loco-location-column)]
                                               [vert-margin loco-text-margin]))
                   (vector-set! loco-info 5 (new choice%
                                               [label #f]	 
                                               [choices (cons "Not set" (map (lambda (dblock)
                                                                               (symbol->string dblock))(sort (hash-keys dblock-draw-data) sort-id-proc)))]	 
                                               [parent (get-panel 'loco-destination-column)]	 
                                               [callback (lambda(choice ev)
                                                           (dest-proc (send (destination(get-loco-data id))get-string-selection)))]
                                               [vert-margin loco-choice-margin]))
                   (vector-set! loco-info 6 (new message%
                                               [label (symbol->string id)]
                                               [parent (get-panel 'instruc-id-panel)]))
                   
                   (vector-set! loco-info 7 (new message%
                                               [label "No destination set so no route calculated"]
                                               [parent (get-panel 'route-panel)]))
                                               
                   (set-loco-data! id loco-info)))

               (define/public (remove-locomotive! id)
                 (let ((loco-data (get-loco-data id)))
                   (send (get-panel 'loco-id-column) delete-child (id-msg loco-data))
                   (send (get-panel 'loco-speed-column) delete-child (speed-slider loco-data))
                   (send (get-panel 'loco-direction-column) delete-child (direction loco-data))
                   (send (get-panel 'loco-manual-column) delete-child (manual loco-data))
                   (send (get-panel 'loco-location-column) delete-child (location loco-data))
                   (send (get-panel 'loco-destination-column) delete-child (destination loco-data))
                   (send (get-panel 'instruc-id-panel) delete-child (id-msg2 loco-data))
                   (send (get-panel 'route-panel) delete-child (route loco-data))))


               (define/public (draw-dblock-info! id occu res)
                 (let ((vct (make-vector 3)))
                   (vector-set! vct 0 (new message%
                                           [label (symbol->string id)]
                                           [parent (get-panel 'dblock-id-column)]
                                           [vert-margin margin]))
                   (vector-set! vct 1 (new message%
                                          [label occu]
                                          [parent (get-panel 'dblock-occupancy-column)]
                                          [vert-margin margin]))
                   (vector-set! vct 2 (new message%
                                           [label res]
                                           [parent (get-panel 'dblock-reservation-column)]
                                           [vert-margin margin]))
                   (set-dblock-data! id vct)))

               (define/public (draw-switches-info! id pos res cb-proc)
                 (let ((vct (make-vector 3)))
                   (vector-set! vct 0 (new message%
                                           [label (symbol->string id)]
                                           [parent (get-panel 'switch-id-column)]
                                           [vert-margin margin]))
                   (vector-set! vct 1 (new choice%
                                               [label #f]	 
                                               [choices (list "1" "2")]	 
                                               [parent (get-panel 'switch-position-column)]	 
                                               [callback (lambda(choice ev)
                                                           (cb-proc (send (position (get-switch-data id))get-selection)))]	 
                                               [selection (- pos 1)]))
                   (vector-set! vct 2 (new message%
                                           [label (if res
                                                      (symbol->string res)
                                                      "Vacant")]
                                           [parent (get-panel 'switch-reservation-column)]
                                           [vert-margin margin]))
                   (set-switch-data! id vct)))
                                               


               (define/public (init-tab-3 add-proc remove-proc)
                 (set-textbox! "add Locomotive ID" (new text-field%
                                                   [label "Locomotive ID"]
                                                   [parent (get-panel 'add-panel)]
                                                   [init-value "Enter Locomotive ID: T-XX"]))
                 (set-textbox! "Previous Dblock" (new text-field%
                                                       [label "Previous Dblock"]
                                                       [parent (get-panel 'add-panel)]
                                                       [init-value "Enter previous Dblock ID"]))
                 (set-textbox! "Current Dblock" (new text-field%
                                                      [label "Current Dblock"]
                                                      [parent (get-panel 'add-panel)]
                                                      [init-value "Enter current Dblock ID"]))
                 (set-button! "Add" (new button%
                                         [label "Add"]
                                         [parent (get-panel 'add-panel)]
                                         [callback (lambda (button e)
                                                     (when (add-proc (string->symbol (send (get-textbox "add Locomotive ID")get-value))
                                                                     (string->symbol (send (get-textbox "Previous Dblock") get-value))
                                                                     (string->symbol (send (get-textbox "Current Dblock") get-value)))
                                                         (send (get-textbox "add Locomotive ID") set-value "Enter Locomotive ID: T-XX")
                                                         (send (get-textbox "Previous Dblock") set-value "Enter previous Dblock ID")
                                                         (send (get-textbox "Current Dblock") set-value "Enter current Dblock ID")))]))
                 (set-textbox! "remove Locomotive ID" (new text-field%
                                                           [label "Locomotive ID"]
                                                           [parent (get-panel 'remove-panel)]
                                                           [init-value "Enter Locomotive ID to be removed"]))
                 (set-button! "Remove" (new button%
                                            [label "Remove"]
                                            [parent (get-panel 'remove-panel)]
                                            [callback (lambda (button e)
                                                        (let* ((id (string->symbol(send (get-textbox "remove Locomotive ID")get-value)))
                                                               (data (get-loco-data id)))
                                                          (when (remove-proc id)
                                                            (hash-remove! loco-draw-data id)
                                                            (send (get-textbox "remove Locomotive ID")
                                                                  set-value "Enter Locomotive ID to be removed"))))])))

                 
               (define/public (init-tab-1 exit-proc)
                 (set-button! "Exit" (new button%
                                          [label "Exit program"]
                                          [parent (get-panel "Menu")]
                                          [callback (lambda (button event)
                                                      (exit-proc))])))

               
               (define/public (init)
                 (init-gui)
                 (send (get-frame 'main-frame) show #t)
                 (send (get-panel 'tab-panel) change-children (lambda (children)
                                                                (list (get-panel "Main"))))
                 (send (get-panel 'tab-panel) change-children (lambda (children)
                                                                (list (get-panel "Menu"))))
                 (set-dialog! 'start-dialog (new dialog%
                                                 [label "Choose a mode"]
                                                 [parent (get-frame 'main-frame)])))


               (define/public (start-gui start-options)
                 (for-each (lambda (el)
                             (new button%
                                  [label (car el)]
                                  [parent (get-dialog 'start-dialog)]
                                  [callback (lambda (button ev)
                                              (begin ((cdr el))
                                                     (send (get-dialog 'start-dialog) show #f)))]))
                           start-options)
                 (send (get-dialog 'start-dialog) show #t))

               (define/public (init-railway locos dblocks switches)
                 switches)
               

               (define/public (exit)
                 (send (get-frame 'main-frame) show #f))))


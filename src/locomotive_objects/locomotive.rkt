#lang racket

(provide locomotive% locomotive?)

(define locomotive% (class object%
                      (init-field id direction location previous-location)
                      (define spd 0)
                      (define destination #f)
                      (define reserved-tracks #f)
                      (define path #f)
                      (define manual-mode #f)
                      (super-new)
                      

                      ;--------------------- private procedures
                      
                      (define/private (direction? input)
                        (or (eq? input 'forward)
                            (eq? input 'backwards)))
                      
                      ;--------------------- public procedures
                      

                      ; changes the reservation to a list of track ids to know which tracks are reserved. 
                      (define/public (reserve! lst)
                        (set! reserved-tracks lst))

                      (define/public (clear-reservations!)
                        (set! reserved-tracks #f))

                      ; returns the ids of the tracks reserved by the locomotive
                      (define/public (made-reservations?) reserved-tracks)

                      ; changes the manual override of the trains
                      (define/public (manual-override boolean)
                        (set! manual-mode boolean))

                      (define/public (manual?)
                        manual-mode)
                       
                      ; returns locomotive id
                      (define/public (get-loco-id) id)

                      ; returns locomotive speed
                      (define/public (get-speed)
                        (if (< spd 0)
                            (- spd)
                            spd))

                      ;; returns the previous-location
                      (define/public (get-previous-location)
                        previous-location)
                       

                      ; sets locomotive speed to input value
                      (define/public (set-speed! new-speed)
                        (if (eq? direction 'forward)
                            (set! spd new-speed)
                            (set! spd (- new-speed))))

                      ; returns the current direction
                      (define/public (get-direction) direction)

                      ; sets the locomotive direction 
                      (define/public (set-direction! new-direction)
                        (when (direction? new-direction)
                          (set! direction new-direction)
                          (set-speed! (get-speed))
                          spd))

                      ; returns the current destination
                      (define/public (get-destination) destination)

                       ; sets the current destination
                      (define/public (set-destination! new-destination)
                        (set! destination new-destination))

                      ; returns current location detectionblock
                      (define/public (get-location) location)

                      ; changes the current-location detectionblock
                      (define/public (set-location! db)
                        (when (and db location)
                          (set! previous-location location))
                        (set! location db))

                      (define/public (set-previous-location! new)
                        (set! previous-location new))

                      ;; returns the path
                      (define/public (get-path)path)

                      ;; changes the next segment
                      (define/public (set-path! list-of-tracks)
                        (set! path list-of-tracks))))
                    


(define (locomotive? object)
  (is-a? object locomotive%))

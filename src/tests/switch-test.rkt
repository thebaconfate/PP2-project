#lang racket

(require rackunit "../railway_objects/switch.rkt")


(test-case
 "switch adt test"
 (define switch (make-object switch% 'S-1 (list '1-1 'U-1 'S-3)))
 (test-begin
  (test-pred "test if switch" switch? switch)
  (test-pred "test if status is a number" integer? (send switch get-status))
  (check-eq? (send switch get-linked-track) 'U-1)
  (send switch set-status! 2)
  (check-eq? (send switch get-status) 2)
  (check-eq? (send switch get-linked-track) 'S-3)))
  
#lang racket

(require rackunit "../railway_objects/detection_block.rkt")

(test-case
 "detection block testing"
 (define dblock (make-object dblock% '1-1 (list 'S-1 '1-2)))
 (test-begin
  (test-pred "is a dblock?" dblock? dblock)
  (check-false (send dblock occupied?))
  (send dblock occupy! (string->symbol "test-train"))
  (check-eq? (send dblock occupied?)(string->symbol "test-train"))
  (send dblock vacant!)
  (check-false (send dblock occupied?))))
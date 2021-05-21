#lang racket

(require rackunit "../nmbs/nmbs.rkt")

(test-case
 "testing nmbs"
 (define nmbs (make-object nmbs% "localhost"))
 (test-begin
  (check-true nmbs? nmbs)
  (send nmbs start-nmbs)))
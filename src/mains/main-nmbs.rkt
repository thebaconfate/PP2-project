#lang racket

(require "../nmbs/nmbs.rkt")

(define nmbs (make-object nmbs%))
(send nmbs start-nmbs)
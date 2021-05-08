#lang racket

(require "../nmbs/nmbs.rkt")


(define nmbs (make-object nmbs% "134.184.232.4"))
(send nmbs start-nmbs)
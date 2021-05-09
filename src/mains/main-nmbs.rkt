#lang racket

(require "../nmbs/nmbs.rkt")


(define nmbs (make-object nmbs% "91.178.162.60"))
(send nmbs start-nmbs)
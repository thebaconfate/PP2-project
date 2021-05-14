#lang racket

(require "../nmbs/nmbs.rkt")


(define nmbs (make-object nmbs% "localhost"))
(send nmbs start-nmbs)
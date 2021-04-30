#lang racket

(require rebellion/type/record)

(define-record-type switch
  (id status)
  #:predicate-name switch?
  #:constructor-name make-switch)

(define (
  
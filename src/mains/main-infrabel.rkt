#lang racket

(require "../infrabel/infrabel.rkt")

(define infrabel (make-object infrabel% #f)) ;; simulator
;; (define infrabel (make-object infrabel% #t)) ;; hardware
(send infrabel start-infrabel)

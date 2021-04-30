#lang racket

(provide send-message request-message send-and-receive)


(define (send-message outport message)
  (unless (port-closed? outport)
    (write message outport)
    (flush-output outport)))

(define (request-message inport)
  (read inport))

(define (send-and-receive inport outport message)
  (send-message outport message)
  (request-message inport))


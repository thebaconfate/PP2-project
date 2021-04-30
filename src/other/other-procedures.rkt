#lang racket

(provide sort-id-proc make-parent-proc path-to-source)


(define sort-id-proc (lambda (el1 el2)
                                       (let* ((id1 (symbol->string el1))
                                              (id2 (symbol->string el2))
                                              (strl1 (string-length id1))
                                              (strl2 (string-length id2)))
                                         (case (string-ref id1 0)
                                           ;; sorts switch ids
                                           ((#\S)(cond
                                                   ((< strl1 strl2)#t)
                                                   ((> strl1 strl2)#f)
                                                   ((= strl1 strl2 3)(< (char->integer (string-ref id1 2))(char->integer(string-ref id2 2))))
                                                   ((= strl1 strl2 4)(if (= (char->integer (string-ref id1 2))(char->integer(string-ref id2 2)))
                                                                         (< (char->integer (string-ref id1 3))(char->integer(string-ref id2 3)))
                                                                         (< (char->integer (string-ref id1 2))(char->integer(string-ref id2 2)))))))                                     
                                           (else (if (= (char->integer (string-ref id1 0))(char->integer(string-ref id2 0)))
                                                     (if (> strl1 2)
                                                         (< (char->integer (string-ref id1 2))(char->integer(string-ref id2 2)))
                                                         (< (char->integer (string-ref id1 1))(char->integer(string-ref id2 1))))
                                                     (< (char->integer (string-ref id1 0))(char->integer(string-ref id2 0)))))))))

(define (make-parent-proc parents-hash)
  (λ (child)(hash-ref parents-hash child)))

;; develops a list with a path to the source from start
(define (path-to-source parents source start)
  (define parent (make-parent-proc parents))
  (let find-path ((element start)
                  (path '()))
    (if (eq? element source)
        (cons element path)
        (find-path (parent element) (cons element path)))))

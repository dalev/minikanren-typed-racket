#lang racket
(require "../main.rkt")
(require rackunit)

(define (number:compare a b)
  (cond [(= a b) 'equal]
        [(< a b) 'less]
        [else 'greater]))

(let ([bindings (build-list 10000 (lambda (n) (cons n (* 2 n))))])
  (define m (map:of-alist bindings #:compare number:compare))
  (check-true
    (match (map:find m 42)
      [(cons 42 84) #t]
      [other 
        (display other)
        #f])))

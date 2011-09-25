#lang racket
(require "../main.rkt")
(require rackunit)

(define numbers (build-list 1000 (lambda (x) x)))

(check-equal? (list:rev-append numbers numbers)
              (append (reverse numbers) numbers))

(check-equal? 
  (for/list ([tl (list:in-tails numbers)])
    (length tl))
  (reverse (map add1 (cons -1 numbers))))

#lang racket/base
(require racket/contract)
(provide/contract
  [prime-factors (integer? . -> . (listof integer?))])

(define (div-2 n) (arithmetic-shift n -1))

(define (square x) (* x x))

;; from http://programmingpraxis.com/2011/09/20/project-euler-problem-3/
(define (prime-factors n)
  (define-values (factors n*)
    ;; pull out as many factors of two as we can
    (let loop ([factors '()]
              [n n])
      (if (even? n)
        (loop (cons 2 factors) (div-2 n))
        (values factors n))))
  ;; then start iterating over remaining possible divisors (all odd since 
  ;; the twos were taken care of)
  (let loop ([factors factors]
             [n n*]
             [divisor 3])
    (define-values (q r) (quotient/remainder n divisor))
    (cond [(= n 1) factors]
          [(> r 0) (loop factors n (+ divisor 2))]
          [else
            (let ([factors (cons divisor factors)]
                  [n q])
              (if (< n (square divisor))
                (cons n factors) ;; done
                (loop factors n divisor)))])))

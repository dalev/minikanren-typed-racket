#lang racket
(require "../main.rkt")
(require rackunit)

(define (number:compare a b)
  (cond [(= a b) 'equal]
        [(< a b) 'less]
        [else 'greater]))

(define (string:compare a b)
  (cond [(string-ci=? a b) 'equal]
        [(string-ci<? a b) 'less]
        [else 'greater]))

(define empty (set:create number:compare))
(check-true (set:empty? empty))

(define 1-to-6 (set:of-list '(1 2 3 4 5 6) #:compare number:compare))
(check-false (set:empty? 1-to-6))

(check-= (set:size empty) 0 0)
(check-= (set:size 1-to-6) 6 0)

(check-equal? (set:to-list 1-to-6) '(1 2 3 4 5 6))

(check-true (set:mem 1-to-6 3))

(check-true (set:for-all? 1-to-6 (lambda (x) (set:mem 1-to-6 x))))
(check-false (set:for-all? 1-to-6 even?))
 
(check-true (set:exists? 1-to-6 even?))

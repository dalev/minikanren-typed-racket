#lang scheme/base
(provide fold)

(define (fold xs #:init init #:f combine)
  (for/fold ([init init]) ([x (in-list xs)]) (combine init x)))

(define (exists? xs #:f ?) (for/or ([x (in-list xs)]) (? x)))

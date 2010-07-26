#lang racket/base
(require racket/contract)
(provide/contract 
  [of-sec  (non-negative-number/c . -> . span?)]
  [of-min  (non-negative-number/c . -> . span?)]
  [of-hour (non-negative-number/c . -> . span?)]
  [of-day  (non-negative-number/c . -> . span?)]
  
  [scale (span? non-negative-number/c . -> . span?)]
  [add (span? span? . -> . span?)])

(define non-negative-number/c
  (flat-named-contract "non-negative-number" (not/c negative?)))

(struct span (seconds))

(define (of-sec seconds) (make-span seconds))
(define (of-min minutes) (of-sec (* minutes 60)))
(define (of-hour hours) (of-min (* hours 60)))
(define (of-day days) (of-hour (* days 24)))

(define (scale span factor)
  (make-span (* factor (span-seconds span))))

(define (add s s*) (make-span (+ (span-seconds s) (span-seconds s*))))

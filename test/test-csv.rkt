#lang racket
(require (prefix-in csv: "../lib/csv.rkt")
         rackunit
         racket/runtime-path)

(define-runtime-path test.csv "test.csv")

(define records1
  (call-with-input-file 
    test.csv
    (λ (in)
      (for/list ([record (csv:in-raw-csv in)])
        record))))

(define records2
  (call-with-input-file 
    test.csv
    (λ (in)
      (for/list ([record (csv:in-raw-csv in #:reverse? #t)])
        record))))

(check-true (> (length records1) 0))

(check-equal? (length records1) (length records2))

(check-true (for/and ([r1 (in-list records1)]
                      [r2 (in-list records2)])
              (equal? r1 (reverse r2))))

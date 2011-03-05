#lang racket
(require (prefix-in csv: "../lib/csv.rkt")
         rackunit
         racket/runtime-path)

(define-runtime-path test.csv "test.csv")

(define records1
  (call-with-input-file 
    test.csv
    (λ (in)
      (for*/list ([records (csv:in-raw-csv-chunks in)]
                  [record  (in-list records)])
        record))))

(define records2
  (call-with-input-file 
    test.csv
    (λ (in)
      (for*/list ([records (csv:in-raw-csv-chunks in #:reverse? #t)]
                  [record  (in-list records)])
        record))))

(check-true (> (length records1) 0))

(check-equal? (length records1) (length records2))

(check-true (for/and ([r1 (in-list records1)]
                      [r2 (in-list records2)])
              (equal? r1 (reverse r2))))

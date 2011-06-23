#lang racket
(require (prefix-in csv: "../lib/csv.rkt")
         rackunit
         racket/runtime-path)

(define-runtime-path test.csv "test.csv")

(define records1
  (call-with-input-file 
    test.csv
    (λ (in)
      (for*/list ([records (csv:in-raw-chunks in)]
                  [record  (in-list records)])
        record))))

(define records2
  (call-with-input-file 
    test.csv
    (λ (in)
      (for*/list ([records (csv:in-raw-chunks in #:reverse? #t)]
                  [record  (in-list records)])
        record))))

(check-true (> (length records1) 0))

(check-equal? (length records1) (length records2))

(check-true (for/and ([r1 (in-list records1)]
                      [r2 (in-list records2)])
              (equal? r1 (reverse r2))))

(define records1-again
  (call-with-input-file
    test.csv
    (λ (in)
      (for/list ([record (csv:in-raw in)])
        record))))

(check-equal? records1 records1-again)

(define records2-again
  (call-with-input-file
    test.csv
    (λ (in)
      (for/list ([rec (csv:in-records in)])
        rec))))
;; CR dalev: finish test case

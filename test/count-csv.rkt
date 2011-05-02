#lang racket
(require (prefix-in csv: "../lib/csv.rkt"))
(require (prefix-in neil: (planet neil/csv:1:6/csv)))

(define make-csv-reader (neil:make-csv-reader-maker '()))

;; About 80s in revision 359156f8400cf0236e87268b641e81ec1cbf2a35
;; on the 1 million row file
(define (main input-file)
  (displayln "Mine:")
  (call-with-input-file
    input-file
    (λ (in)
      (displayln 
        (time 
          (for/fold ([sum 0]) ([rows (csv:in-raw-csv-chunks in)])
            (+ sum (length rows)))))))

  ;(displayln "Neil's:")
  #;
  (call-with-input-file
    input-file
    (λ (in)
      (define next-row (make-csv-reader in))
      (displayln 
        (time 
          (for/last ([_row (in-producer next-row '())]
                     [n (in-naturals 1)])
            n))))))

(match (current-command-line-arguments)
  [(vector input) (main input)])

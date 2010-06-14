#lang scheme/base

;; degenerate implementation
(define (read-csv-row port)
  (let ([line (read-line)])
    (if (eof-object? line)
      line
      (regexp-split #rx"," line))))

(with-input-from-file (expand-user-path (build-path "~" "test.csv"))
  (lambda ()
    (for ([row (in-port read-csv-row)])
      (display row)
      (newline))))

#lang racket
(require (planet jaymccarthy/sqlite)
         (planet dalev/core/lib/string))

(provide (all-defined-out))

(define (csv->db csv-path #:db [db ':memory:])
  (define *db* (open db))
  (exec/ignore *db* "create table temp_csv(a integer, b text)")
  (define insert-stmt
    (prepare *db* "insert into temp_csv(a, b) values(?, ?)"))
  (with-transaction (*db* _fail)
    (call-with-input-file
      csv-path
      (λ (in)
        (for ([line (in-lines in)])
          (match-define (list a b) (split #:on #\, line))
          (run insert-stmt a b)))))
  (finalize insert-stmt)
  *db*)

;; For testing
(define (create-words-csv)
  (call-with-input-file
    (build-path "/" "usr" "share" "dict" "words")
    (λ (in)
      (for ([word (in-port read-line in)])
        (printf "~a,~a~n" (string-length word) word)))))

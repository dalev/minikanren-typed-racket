#lang racket
(require (planet jaymccarthy/sqlite)
         (planet dalev/core/lib/string))

(provide (all-defined-out))

;; CR dalev: use file header to determine columns.
;; CR dalev: split isn't really appropriate.  sigh.
(define (csv->db csv-path #:db [db ':memory:])
  (define *db* (open db))
  (exec/ignore *db* "create table temp_csv(a integer, b text)")
  (define insert-stmt
    (prepare *db* "insert into temp_csv(a, b) values(?, ?)"))
  (with-transaction (*db* abort-txn)
    (call-with-input-file
      csv-path
      (λ (in)
        (for ([line (in-lines in)])
          (match (split #:on #\, line)
            [(list a b) (run insert-stmt a b)]
            [_ (abort-txn)])))))
  (finalize insert-stmt)
  *db*)

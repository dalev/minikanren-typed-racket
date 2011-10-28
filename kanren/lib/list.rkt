#lang racket/base
(require "../main.rkt")

(provide (all-defined-out))

(define (%car p a) (exist (d) (== (cons a d) p)))
(define (%cdr p d) (exist (a) (== (cons a d) p)))
(define (%cons a d p) (== (cons a d) p))
(define (%null x) (== '() x))
(define (%pair p) (exist (a d) (%cons a d p)))

(define (%list l)
  (conde
    [(%null l) succeed]
    [(%pair l)
      (exist (d)
        (%cdr l d)
        (%list d))]
    [else fail]))

(define (%member x l)
  (conde
    [(%null l) fail]
    [(%car l x) succeed]
    [else
      (exist (d)
        (%cdr l d)
        (%member x d))]))

(define (%rember x l out)
  (conde
    [(%null l) (%null out)]
    [(%car l x) (%cdr l out)]
    [else (exist (a d res)
            (%cons a d l)
            (%cons a res out)
            (%rember x d res))]))

(define (%append l s out)
  (conde
    [(%null l) (== s out)]
    [else 
      (exist (a d res)
        (%cons a d l)
        (%cons a res out)
        (%append d s res))]))


#lang racket/base
(require "../main.rkt")

(provide (all-defined-out))

(define (%car p a) (exist (d) (== (cons a d) p)))
(define (%cdr p d) (exist (a) (== (cons a d) p)))
(define (%cons p a d) (== (cons a d) p))
(define (%null x) (== '() x))
(define (%pair p) (exist (a d) (%cons p a d)))

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
            (%cons l a d)
            (%cons out a res)
            (%rember x d res))]))

(define (%append l s out)
  (conde
    [(%null l) (== s out)]
    [else 
      (exist (a d res)
        (%cons l a d)
        (%cons out a res)
        (%append d s res))]))


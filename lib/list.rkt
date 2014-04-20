#lang racket/base
(require "kanren.rkt")

(provide (all-defined-out))

(define (%car p a) (fresh (d) (== (cons a d) p)))
(define (%cdr p d) (fresh (a) (== (cons a d) p)))
(define (%cons p a d) (== (cons a d) p))
(define (%null x) (== '() x))
(define (%pair p) (fresh (a d) (%cons p a d)))

(define (%first l fst) (%car l fst))
(define (%second l snd)
  (fresh (tail)
    (%cdr l tail)
    (%car tail snd)))

(define (%list l)
  (conde
    [(%null l) succeed]
    [(%pair l)
      (fresh (d)
        (%cdr l d)
        (%list d))]
    [else fail]))

(define (%member x l)
  (conde
    [(%null l) fail]
    [(%car l x) succeed]
    [else
      (fresh (d)
        (%cdr l d)
        (%member x d))]))

(define (%rember x l out)
  (conde
    [(%null l) (%null out)]
    [(%car l x) (%cdr l out)]
    [else (fresh (a d res)
            (%cons l a d)
            (%cons out a res)
            (%rember x d res))]))

(define (%append l s out)
  (conde
    [(%null l) (== s out)]
    [else 
      (fresh (a d res)
        (%cons l a d)
        (%cons out a res)
        (%append d s res))]))


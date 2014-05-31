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
        (%list d))]))

(define (%member l x)
  (conde
    [(%null l) fail]
    [(%car l x) succeed]
    [succeed
      (fresh (d)
        (%cdr l d)
        (%member d x))]))

(define (%append l s out)
  (conde
    [(%null l) (== s out)]
    [succeed 
      (fresh (a d res)
        (%cons l a d)
        (%cons out a res)
        (%append d s res))]))


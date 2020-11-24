#lang racket/base
(require "../main.rkt")

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
    [(%null l)]
    [(%pair l)
      (fresh (d)
        (%cdr l d)
        (%list d))]))

(define (%member l x)
  (conde
    [(%null l) fail]
    [(%car l x)]
    [(fresh (d)
       (%cdr l d)
       (%member d x))]))

(define (%append l s out)
  (conde
    [(%null l) (== s out)]
    [(fresh (a d res)
       (%cons l a d)
       (%cons out a res)
       (%append d s res))]))

(module+ test
  (require rackunit)
  (check-equal?
    (run* (xs ys) (%append xs ys '(1 2 3)))
    (list 
      (list '() '(1 2 3))
      (list '(1) '(2 3))
      (list '(1 2) '(3))
      (list '(1 2 3) '()))))

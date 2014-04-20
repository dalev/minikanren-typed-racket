#lang racket/base
(require "../lib/kanren.rkt"
         (prefix-in U. rackunit))

(struct fish (color weight) #:transparent)
(struct my-pair (a b) #:transparent)

(U.check-equal?
  (run* (p) 
    (fresh (x y)
      (== p (my-pair x y))
      (== (fish 'red 10) (fish x y))))
  (list (list (my-pair 'red 10))))

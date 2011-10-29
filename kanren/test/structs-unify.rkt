#lang racket/base
(require "../main.rkt"
         (prefix-in U. rackunit))

(struct fish (color weight) #:transparent)
(struct my-pair (a b) #:transparent)

(U.check-equal?
  (run* (p) 
    (exist (x y)
      (== p (my-pair x y))
      (== (fish 'red 10) (fish x y))))
  (list (my-pair 'red 10)))

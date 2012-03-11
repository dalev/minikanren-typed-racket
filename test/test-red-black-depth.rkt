#lang racket
(require (prefix-in tree: "../lib/red-black-tree.ss"))
(require rackunit)

(define (number:compare a b)
  (cond [(= a b) 'equal]
        [(< a b) 'less]
        [else 'greater]))

(define *size* 5000)

(define log2-*size* (/ (log *size*) (log 2)))

(let* ([tree (for/fold ([tree tree:empty]) ([e (in-range 0 *size*)]) 
               (check-true (tree:invariant? tree))
               (tree:add tree e number:compare))]
       [depth (tree:depth tree)])
  (check-true (>= depth log2-*size*))
  (check-true (<= depth (* 2 log2-*size*))))

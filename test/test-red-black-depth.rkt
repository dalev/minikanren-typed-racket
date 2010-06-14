#lang scheme
(require (prefix-in tree: "../lib/red-black-tree.ss"))
(require (planet cce/fasttest:3:5/random)
         (planet cce/fasttest:3:5/schemeunit)
         (planet schematics/schemeunit:3:4))

(define (number:compare a b)
  (cond [(= a b) 'equal]
        [(< a b) 'less]
        [else 'greater]))

(define *size* 50000)

(define log2-*size* (/ (log *size*) (log 2)))

(let* ([tree (for/fold ([tree tree:leaf]) ([e (in-range 0 *size*)]) 
               (tree:add tree e number:compare))]
       [depth (tree:depth tree)])
  (check-true (>= depth log2-*size*))
  (check-true (<= depth (* 2 log2-*size*))))

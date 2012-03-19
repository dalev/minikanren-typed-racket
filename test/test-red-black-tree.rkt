#lang racket
(require (prefix-in tree: "../lib/red-black-tree.ss"))
(require rackunit rackunit/text-ui)

(run-tests tree:tests)

(define (number:compare a b)
  (cond [(= a b) 'equal]
        [(< a b) 'less]
        [else 'greater]))

(define *size* 500)

(define log2-*size* (/ (log *size*) (log 2)))

(define (vector-swap! v i j)
  (define tmp (vector-ref v i))
  (vector-set! v i (vector-ref v j))
  (vector-set! v j tmp))

(define (vector-shuffle! v)
  (for ([i (in-range (vector-length v) 0 -1)])
    (vector-swap! v (sub1 i) (random i))))

(define numbers (build-vector *size* identity))
(vector-shuffle! numbers)

(let ()
  (define tree 
    (for/fold ([tree tree:empty]) ([e (in-vector numbers)]) 
      (check-true (tree:invariant? tree))
      (tree:add tree e number:compare)))
  (define depth (tree:depth tree))

  (check-true (>= depth log2-*size*))
  (check-true (<= depth (* 2 log2-*size*)))

  (vector-shuffle! numbers)

  (define tree^
    (for/fold ([tree tree]) ([e (in-vector numbers)])
      (check-true (tree:invariant? tree))
      (tree:mem tree e number:compare)
      (tree:remove tree e number:compare)))

  (check-true (tree:empty? tree^))
  )

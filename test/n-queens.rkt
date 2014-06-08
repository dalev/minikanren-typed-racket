#lang racket/base
(require "../main.rkt"
         racket-core/lib/list-untyped
         (only-in racket/stream
                  stream->list))

#| 
A brute-force N-queens solver -- transliterated from the solution using
finite-domain constraints presented in 

  _cKanren: miniKanren with Constraints_ 

by Alvis et al in the 2011 Scheme workshop.  I've replaced the FD range
constraints with explicit [%member] calls, and the FD `all-different'
constraint is expanded into the O(n^2) disunification constraints.
|#
(define (n-queens% q* n)
  (define columns (for/list ([i (in-range n)]) i))
  (let loop ([i n] [vars '()])
    (if (zero? i)
      (all
        (== q* vars)
        (all-different% vars)
        (diagonals% n vars))
      (fresh (x)
        (%member columns x)
        (loop (- i 1) (cons x vars))))))

(define (all-different% vars)
  (for/fold ([goal succeed]) ([v (in-list vars)]
                              [tl (in-tails (cdr vars))])
    (for/fold ([goal goal]) ([v^ (in-list tl)])
      (all goal (=/= v v^)))))

#| This is a bit slower than the above implementation
(define (all-different% xs)
  (conde
    [(== xs '()) succeed]
    [(fresh (x) (== xs (list x))) succeed]
    [else
      (fresh (fst snd rest)
        (== xs (cons fst (cons snd rest)))
        (=/= fst snd)
        (all-different% (cons fst rest))
        (all-different% (cons snd rest)))]))
|#

(define (diagonals% n vars)
  (let loop ([r vars] [i 0] [s (cdr vars)] [j 1])
    (cond
      [(or (null? r) (null? (cdr r))) succeed]
      [(null? s) (loop (cdr r) (+ i 1) (cddr r) (+ i 2))]
      [else
        (let ([q@i (car r)]
              [q@j (car s)])
          (all (diagonal% q@i q@j (- j i) (stream->list (in-range 0 (* 2 n))))
               (loop r i (cdr s) (+ j 1))))])))

(define (diagonal% q@i q@j d range)
  (fresh (q@i+d q@j+d)
    (%member range q@i+d)
    (%member range q@j+d)
    (=/= q@i+d q@j)
    (=/= q@j+d q@i)
    (project (q@i q@j d)
      (== q@i+d (+ q@i d))
      (== q@j+d (+ q@j d)))))

(module+ test
  (require rackunit)

  (define expected-number-of-solutions
    (list 1 0 0 2 10 4 40 92))

  ;; On my macbook air, 
  ;; n = 5 takes about 960 ms
  ;; n = 6 takes about 11.7 seconds
  ;; n = 7 takes about 190 seconds.

  (for ([expected-n (in-list expected-number-of-solutions)]
        [i (in-naturals 1)])
    (check-equal? (run* (queens) (n-queens% queens i))
                  expected-n)))

(module+ main
  (require racket/match)
  (define dimension
    (match (current-command-line-arguments)
      [(vector) 4]
      [(vector dimension) (string->number dimension)]
      [other (raise-user-error "Expected 0 or 1 argument, but got"
                               (vector-length other))]))
  (time
    (run* (queens) 
      (n-queens% queens dimension))))

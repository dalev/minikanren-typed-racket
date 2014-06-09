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
(define (n-queens% queen-columns n)
  (define columns (for/list ([i (in-range n)]) i))
  ;; the i'th variable represents the queen in row i
  (let loop ([i n] [vars '()])
    (if (zero? i)
      (all
        (== queen-columns vars)
        (all-different% vars)
        (no-shared-diagonals% vars))
      (fresh (x)
        (%member columns x)
        (loop (- i 1) (cons x vars))))))

#| Hack alert:
[xs] is a _racket_ list of values; this is technically not a kanren relation
in the sense that [xs] is not a kanren logic variable associated with some
value via the substitution.

The procedure constructs the desired disunification goal in one pass rather 
than inferring it incrementally like the "purer" implementation below.
|#
(define (all-different% xs)
  (for/fold ([goal succeed]) ([x (in-list xs)]
                              [tl (in-tails (cdr xs))])
    (for/fold ([goal goal]) ([x^ (in-list tl)])
      (all goal (=/= x x^)))))

#| This is a "pure" implementation of [all-different%], but it makes the
   solver run a bit slower than using the above hack.
   (I observed a factor of about 2.5 for n = 6 or n = 7)

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

;; Again, [vars] is a _racket_ list, not a logic variable associated with a list.
;; CR dalev: can we write a "pure kanren" version?  how well does it perform?
(define (no-shared-diagonals% vars)
  (let loop ([r vars] [i 0] [s (cdr vars)] [j 1])
    (cond
      [(or (null? r) (null? (cdr r))) succeed]
      [(null? s) (loop (cdr r) (+ i 1) (cddr r) (+ i 2))]
      [else
        (let ([q@i (car r)]
              [q@j (car s)])
          (all (not-on-diagonal% q@i q@j (- j i))
               (loop r i (cdr s) (+ j 1))))])))

;; [d] is the number of rows that separate q@i and q@j, and therefore it is
;; also the number of columns that would separate them if they shared a diagonal
(define (not-on-diagonal% q@i q@j d)
  (fresh (q@i+d q@j+d)
    (project (q@i q@j d)
      (== q@i+d (+ q@i d))
      (== q@j+d (+ q@j d))
      (=/= q@i+d q@j)
      (=/= q@j+d q@i))))

(module+ test
  (require rackunit)

  (define expected-number-of-solutions
    (list 1 0 0 2 10 4 40 92))

  (for ([expected-n (in-list expected-number-of-solutions)]
        [i (in-naturals 1)]
        ;; On my macbook air, 
        ;; n = 5 takes about 79 ms
        ;; n = 6 takes about 1700 ms
        ;; n = 7 takes about 38 seconds.
        #:when (<= i 7))
    (check-equal? (length (run* (queens) (n-queens% queens i)))
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

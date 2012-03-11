#lang racket/base
(require racket/match
         racket/contract
         (prefix-in tree. "red-black-tree.rkt"))

(define bool/c (one-of/c #t #f))
(define comparator/c (any/c any/c . -> . (symbols 'less 'equal 'greater)))
(struct set (elts compare))

(provide/contract 
  [create (comparator/c . -> . set?)]
  [empty? (set? . -> . bool/c)]
  [add (set? any/c . -> . set?)]
  [mem (set? any/c . -> . bool/c)]
  [for-all? (set? (any/c . -> . bool/c) . -> . bool/c)]
  [exists? (set? (any/c . -> . bool/c) . -> . bool/c)]
  [size (set? . -> . natural-number/c)]
  [of-list ((listof any/c) #:compare comparator/c . -> . set?)]
  [to-list (set? . -> . (listof any/c))])

(define (create compare) (set tree.empty compare))

(define (empty? set) (tree.empty? (set-elts set)))

(define (add set e)
  (let ([compare (set-compare set)])
    (set (tree.add (set-elts set) e (set-compare set)) compare)))

(define (mem a-set e)
  (match a-set
    [(set tree compare) (tree.mem tree e compare)]))

(define (for-all? a-set p?)
  (match a-set
    [(set tree _)
     (let/ec return
       (tree.iter tree (lambda (x) (unless (p? x) (return #f))))
       #t)]))

(define (exists? a-set p?)
  (match a-set
    [(set tree _)
     (let/ec return
       (tree.iter tree (lambda (x) (when (p? x) (return #t))))
       #f)]))

(define (size set) (tree.size (set-elts set)))

(define (of-list elts #:compare compare)
  (set 
    (for/fold ([tree tree.empty]) ([e (in-list elts)])
      (tree.add tree e compare))
    compare))

(define (to-list set) (tree.to-list (set-elts set)))

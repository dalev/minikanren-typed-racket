#lang racket/base
(require racket/match
         racket/contract
         (prefix-in tree. "red-black-tree.rkt"))

(define bool/c (one-of/c #t #f))

(define comparator/c (any/c any/c . -> . (symbols 'less 'equal 'greater)))

(define-struct set (elts compare))

(provide/contract [create (comparator/c . -> . set?)])
(define (create compare) (make-set tree.empty compare))

(provide/contract [add (set? any/c . -> . set?)])
(define (add set e)
  (let ([compare (set-compare set)])
    (make-set (tree.add (set-elts set) e (set-compare set)) compare)))

(provide/contract [mem (set? any/c . -> . bool/c)])
(define (mem a-set e)
  (match a-set
    [(struct set (tree compare)) (tree.mem tree e compare)]))

(provide/contract [for-all? (set? (any/c . -> . bool/c) . -> . bool/c)])
(define (for-all? a-set p?)
  (match a-set
    [(struct set (tree _))
     (let/ec return
       (tree.iter tree (lambda (x) (unless (p? x) (return #f))))
       #t)]))

(provide/contract [exists? (set? (any/c . -> . bool/c) . -> . bool/c)])
(define (exists? a-set p?)
  (match a-set
    [(struct set (tree _))
     (let/ec return
       (tree.iter tree (lambda (x) (when (p? x) (return #t))))
       #f)]))

(provide/contract [size (set? . -> . natural-number/c)])
(define (size set) (tree.size (set-elts set)))

(provide/contract [of-list ((listof any/c) #:compare comparator/c . -> . set?)])
(define (of-list elts #:compare compare)
  (make-set 
    (for/fold ([tree tree.empty]) ([e (in-list elts)])
      (tree.add tree e compare))
    compare))

(provide/contract [to-list (set? . -> . (listof any/c))])
(define (to-list set) (tree.to-list (set-elts set)))

#lang typed/racket/base
(provide
  Goal
  Stream
  Subst

  call/fresh
  ==
  disj2
  conj2
  fail
  sbral-empty

  reify-1st
  call/goal
  pull
  take
  take-all)

(require racket/match)

(require/typed racket/fixnum [fx+ (-> Fixnum Fixnum Fixnum)])

(require "skew-bral.rkt")
(define-type Sbral (sbral Any))

(module+ test (require typed/rackunit))

(define-type Subst (Pair Sbral Fixnum))

(define-type Stream 
             (U Null 
                (Pair Subst Stream)
                (-> Stream)))

(define-type Goal (-> Sbral Fixnum Stream))

(struct var [{index : Fixnum}] #:transparent)

(: var=? : var var -> Boolean)
(define (var=? x y) (= (var-index x) (var-index y)))

(: walk : Any Sbral -> Any)
(define (walk u s)
  (let ([pr (and (var? u) (sbral-ref s (var-index u)))])
    (cond 
      [(not pr) u]
      [(eq? pr u) u]
      [else (walk pr s)])))

(: ext-s : var Any Sbral -> Sbral)
(define (ext-s x v s) 
  (sbral-set s (var-index x) v))

(: == : Any Any -> Goal)
(define (== u v)
  (lambda (s c)
    (let ((s (unify u v s)))
      (if s 
        (unit s c) 
        mzero))))

(: unit : Sbral Fixnum -> Stream)
(define (unit s c) (cons (cons s c) mzero))

(: mzero : Stream)
(define mzero '())

(: fail : Goal)
(define fail (lambda (_s _c) mzero))

(: unify : Any Any Sbral -> (Option Sbral))
(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(: call/fresh : (-> var Goal) -> Goal)
(define (call/fresh f)
  (lambda (s c)
    (define x (var c))
    ((f x) (sbral-cons x s) (fx+ c 1))))

(: disj2 : Goal Goal -> Goal)
(define (disj2 g1 g2) (lambda (s c) (mplus (g1 s c) (g2 s c))))

(: conj2 : Goal Goal -> Goal)
(define (conj2 g1 g2) (lambda (s c) (bind (g1 s c) g2)))

(: mplus : Stream Stream -> Stream)
(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(: bind : Stream Goal -> Stream)
(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else 
      (let ([fst (car $)])
        (mplus (g (car fst) (cdr fst)) 
               (bind (cdr $) g))))))

(: reify-1st : (Pair Sbral Fixnum) -> Any)
(define (reify-1st s/c)
  (let ((v (walk* (var 0) (car s/c))))
    v))

(: walk* : Any Sbral -> Any)
(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s)))
      (else  v))))

(: reify-name : Fixnum -> Symbol)
(define (reify-name n)
  (string->symbol
    (string-append "_" "." (number->string n))))

(: call/goal : Goal -> Stream)
(define (call/goal g) (g sbral-empty 0))

(: pull : Stream -> (U Null (Pair Subst Stream)))
(define (pull $)
  (if (procedure? $) (pull ($)) $))

(: take-all : Stream -> (Listof Subst))
(define (take-all $)
  (let (($ (pull $)))
    (if (null? $) '() (cons (car $) (take-all (cdr $))))))

(: take : Integer Stream -> (Listof Subst))
(define (take n $)
  (if (zero? n) '()
    (let (($ (pull $)))
      (if (null? $) '() (cons (car $) (take (- n 1) (cdr $)))))))

(module+ test
  (check-equal?
    (sbral-ref (sbral-cons 17 sbral-empty) 0)
    17)

  (check-equal?
    (walk (var 0) (sbral-cons 17 sbral-empty))
    17)

  (check-equal?
    (let ([stream 
            ((call/fresh (lambda (x) (== x 42)))
             sbral-empty
             0)])
      (match stream
        [(cons fst _) (reify-1st fst)]
        [_ (error "Expected a cons")]))
    42))


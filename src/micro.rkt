#lang racket/base
(provide
  call/fresh
  ==
  disj
  conj
  fail
  empty-state

  reify-1st)

(require "skew-bral.rkt"
         racket/match)

(module+ test (require rackunit))

(define empty-state (cons sbral-empty 0))

(struct var [index])

(define (var=? x y) (= (var-index x) (var-index y)))

(define (walk u s)
  (let ([pr (and (var? u) (sbral-ref s (var-index u)))])
    (cond 
      [(not pr) u]
      [(eq? pr u) u]
      [else (walk pr s)])))

(define (ext-s x v s) 
  (sbral-set s (var-index x) v))

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s 
        (unit (cons s (cdr s/c))) 
        mzero))))

(define (unit s/c) (cons s/c mzero))

(define mzero '())

(define fail (lambda (_) mzero))

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

(define (call/fresh f)
  (lambda (s/c)
    (match-define (cons s c) s/c)
    (define x (var c))
    ((f x) (cons (sbral-cons x s) (+ c 1)))))

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

(define (reify-1st s/c)
  (let ((v (walk* (var 0) (car s/c))))
    v))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s)))
      (else  v))))

(define (reify-name n)
  (string->symbol
    (string-append "_" "." (number->string n))))

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
             empty-state)])
      (map reify-1st stream))
    (list 42)))


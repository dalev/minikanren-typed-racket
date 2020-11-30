#lang typed/racket/base
(provide Term
         Subst
         (struct-out var)
         subst-new-var
         subst-empty
         unify
         walk
         walk*)

(require "skew-bral.rkt")

(struct var [{index : Integer}] #:transparent)

(: var=? : var var -> Boolean)
(define (var=? x y) (= (var-index x) (var-index y)))

(define-type Term (U var
                     Null (Pair Term Term)
                     Number
                     Symbol))

(define-type Subst (sbral Term))

(define subst-empty sbral-empty)

(: subst-new-var : Subst -> (Values var Subst))
(define (subst-new-var sub)
  (let ([c (sbral-size sub)])
    (define v (var c))
    (values v (sbral-cons v sub))))

(: unify : Term Term Subst -> (Option Subst))
(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      [(and (var? u) (var? v) (var=? u v)) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s)))]
      [else (and (eqv? u v) s)])))

(: walk : Term Subst -> Term)
(define (walk u s)
  (let ([pr (and (var? u) (sbral-ref s (var-index u)))])
    (cond 
      [(not pr) u]
      [(eq? pr u) u]
      [else (walk pr s)])))

(: walk* : Term Subst -> Term)
(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s))]
      [else  v])))

(: ext-s : var Term Subst -> Subst)
(define (ext-s x v s) 
  (sbral-set s (var-index x) v))


#lang typed/racket/base
(provide Term
         State
         state-empty
         (struct-out var)
         state-new-var
         unify
         reify)

(require "skew-bral.rkt")

(struct var [{index : Integer}] #:transparent)

(: var=? : var var -> Boolean)
(define (var=? x y) (= (var-index x) (var-index y)))

(define-type Term (U var
                     Null (Pair Term Term)
                     Number
                     Symbol))

(define-type Subst (sbral Term))

(struct State [{subst : Subst}] #:transparent)

(: state-empty : -> State)
(define (state-empty) (State subst-empty))

(define subst-empty sbral-empty)

(: state-new-var : State -> (Values var State))
(define (state-new-var s)
  (define sub (State-subst s))
  (let ([c (sbral-size sub)])
    (define v (var c))
    (values v (State (sbral-cons v sub)))))

(: reify : Term State -> Term)
(define (reify term state) 
  (let ([subst (State-subst state)])
    (walk* term subst)))

(: unify : Term Term State -> (Option State))
(define (unify u v s)
  (define subst
    (let loop : (Option Subst) ([u u] [v v] [s (State-subst s)])
      (let ([u (walk u s)] [v (walk v s)])
        (cond
          [(and (var? u) (var? v) (var=? u v)) s]
          [(var? u) (ext-s u v s)]
          [(var? v) (ext-s v u s)]
          [(and (pair? u) (pair? v))
           (let ((s (loop (car u) (car v) s)))
             (and s (loop (cdr u) (cdr v) s)))]
          [else (and (eqv? u v) s)]))))
  (and subst (State subst)))

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


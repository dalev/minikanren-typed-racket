#lang typed/racket
(provide
  run*
  fresh
  ==
  conde
  conj
  disj
  fail

  Term
  Goal)

#| This module implements minikanren using a dual-continuation representation
   of the backtracking monad.  It is an adaptation of Kiselyov et al's ICFP 2005
   paper "Backtracking, Interleaving, and Terminating Monad Transformers".
|#

(require racket/match
         "term.rkt")

;; (T a) represents a "stream of a" using a dual-contination encoding.
;; As in the ICFP 2005 paper, our representation is fully polymorphic in
;; the observation type.
(define-type (FK o) (-> o))
(define-type (SK o a) (-> a (FK o) o))
(define-type (T a) (All (o) (-> (SK o a) (FK o) o)))

;; Representation a computation that succeeds exactly once.
(: unit : (All (a) (-> a (T a))))
(define (unit x)
  (lambda (k f) 
    (k x f)))

;; The standard "map-append" operation.
(: bind : (All (a b) (-> (T a) (-> a (T b)) (T b))))
(define #:forall (a b) bind 
  (lambda ({t : (T a)} g)
    (lambda #:forall (o) ({k : (SK o b)} {f : (FK o)})
      (t (lambda ({x : a} {f : (FK o)}) 
           ((g x) k f))
         f))))

;; A computation with no results.
(: mzero : (All (a) (T a)))
(define mzero (lambda (_k f) (f)))

;; A representatin of the "append" of the two given streams.
;; This implementation is not fair: it produces answers left-to-right.
(: mplus : (All (a) (-> (T a) (T a) (T a))))
(define (mplus lhs rhs)
  (lambda (k f)
    (lhs k (lambda () (rhs k f)))))

(define-type (Opt-Split a) (Option (Pair a (T a))))

;; `msplit` is the essential bit of cleverness from the ICFP 2005 paper:
;; it shows how to split a `(T a)` into a "first" and a "rest", without having to
;; fully observe the entire rest of the stream.  The trick is to observe the 
;; given value `t` at the concrete type (Opt-Split a) (by choosing appropriate 
;; success and failure continuations, called `ssk` and `ffk` below).
;; If `ssk` observes `t`, then it uses a further inversion of control
;; recover the fully-polymorphic answer type.
(: msplit : (All (a) (-> (T a) (T (Opt-Split a)))))
(define msplit 
  (lambda #:forall (a) ({t : (T a)})
    (define-type Obs (Opt-Split a))
    (: ssk : (SK (T Obs) a))
    (define (ssk v {fk : (FK (T Obs))})
      (lambda (a b)
        (a (cons
            v 
            (lambda #:forall (o) ({s* : (SK o a)} {f* : (FK o)})
              ((fk)
               (lambda ({obs : Obs} {h : (FK o)}) 
                 (match obs
                   [#f (h)]
                   [(cons v** x)
                    (s* v** (lambda () (x s* f*)))]))
               f*)))
           b)))
    (: ffk : (FK (T Obs)))
    (define (ffk) ((inst unit Obs) #f))
    (t ssk ffk)))

(: interleave : (All (a) (T a) (T a) -> (T a)))
(define (interleave lhs rhs)
  ((inst bind (Opt-Split a) a)
   ((inst msplit a) lhs)
   (lambda ({obs : (Opt-Split a)})
     (match obs
       [#f rhs]
       [(cons lhs-fst lhs-rest)
        ((inst mplus a) 
         ((inst unit a) lhs-fst)
         ((inst interleave a) rhs lhs-rest))]))))

;; Like (bind m g), but interleaves the streams (g a), for a in [m]
(: bind/fair : (All (a b) (-> (T a) (-> a (T b)) (T b))))
(define (bind/fair t g)
  ((inst bind (Opt-Split a) b) 
   ((inst msplit a) t) 
   (lambda ({obs : (Opt-Split a)})
     (match obs
       [#f (inst mzero b)]
       [(cons fst rest)
        ((inst interleave b) (g fst) ((inst bind/fair a b) rest g))]))))

(define-type Goal (-> State Stream))

(define-type Stream (T State))

(: goal->stream : State Goal -> Stream)
(define (goal->stream state goal)
  (goal state))

(define fail (== 0 1))

(define-syntax fresh
  (syntax-rules ()
    [(_ (x ...) g0 g ...)
     (lambda ({state : State})
       (let*-values ([{x state} (state-new-var state)] 
                     ...) 
         (goal->stream state (conj g0 g ...))))]))

(define-syntax ==
  (syntax-rules ()
    [(_ lhs rhs)
     (lambda ({st : State})
       (let ([st (unify lhs rhs st)])
         (if st
           ((inst unit State) st)
           (inst mzero State))))]))

(define-syntax conj
  (syntax-rules ()
    [(_ g) g]
    [(_ g0 g ...) 
     (lambda ({st : State})
       ((inst bind/fair State State)
        (goal->stream st g0) 
        (lambda ({st : State})
          (goal->stream st (conj g ...)))))]))

(define-syntax disj
  (syntax-rules ()
    [(_ g) g]
    [(_ g0 g ...) 
     (lambda ({st : State})
       ((inst interleave State)
        (goal->stream st g0) 
        (goal->stream st (disj g ...))))]))

(define-syntax conde
  (syntax-rules ()
    [(_ (g0 g ...) ...) (disj (conj g0 g ...) ...)]))

(define-syntax (define-relation stx)
  (syntax-case stx ()
    [(_ (rel x ...) g0 g ...)
     (let ([terms (lambda (s) (map (lambda (_) #'Term) (syntax->list s)))])
       (with-syntax ([(x-ty ...) (terms #'(x ...))])
         (syntax 
           (begin
             (: rel : (-> x-ty ... Goal))
             (define rel 
               (lambda (x ...)
                 (conj g0 g ...)))))))]))

(: stream->list : (All (a) (-> (T a) (Listof a))))
(define (stream->list s)
  (((inst msplit a) s) 
   (lambda ({obs : (Opt-Split a)} _) 
     (match obs
       [#f null]
       [(cons fst snd)
        (cons fst ((inst stream->list a) snd))]))
   (lambda () null)))

(define state-stream->list (inst stream->list State))

(define-syntax run*
  (syntax-rules ()
    [(_ (x ...) g0 g ...)
     (run* q (fresh (x ...) 
               (== q (list x ...)) 
               g0 g ...))]
    [(_ q g0 g ...)
     (let* ([goal : Goal (fresh (q) g0 g ...)]
            [stream : Stream (goal->stream (state-empty) goal)])
       (let ([q (var 0)])
         (map (lambda ({s : State}) : Term (reify q s)) 
              (state-stream->list stream))))]))

(module+ test
  (require typed/rackunit)

  (let* ([unit (inst unit Integer)]
         [mplus (inst mplus Integer)]
         [interleave (inst interleave Integer)]
         [mzero (inst mzero Integer)]
         [stream->list (inst stream->list Integer)])
    (check-equal? 
      (stream->list (interleave mzero (mplus (unit 1) (mplus (unit 2) (unit 3)))))
      (list 1 2 3))
    (check-equal? 
      (stream->list (interleave (mplus (unit 1) (mplus (unit 2) (unit 3))) mzero))
      (list 1 2 3)))

  (define-relation (append xs ys zs)
     (conde 
       [(== xs '()) (== ys zs)]
       [(fresh (x xs* tmp)
          (== (cons x xs*) xs)
          (== zs (cons x tmp))
          (append xs* ys tmp))]))

  (let ([ts (run* (xs ys) (append xs ys '(1 2 3)))])
    (check-equal?
      ts
      (list
        (list '() '(1 2 3))
        (list '(1) '(2 3))
        (list '(1 2) '(3))
        (list '(1 2 3) '())))))

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

(require racket/match
         "term.rkt")

(struct State [{subst : Subst}] #:transparent)

(define *initial-state* (State subst-empty))

(define-type Goal (U Disj2 Conj2 == Call-with-state))

(define-type Stream (U Bind Mplus Pause
                       Null
                       (Pair State Stream)))

(: stream-ready? : Stream -> Boolean)
(define (stream-ready? s)
  (match s
    ['() #t]
    [(cons _1 _2) #t]
    [_ #f]))

(struct Disj2 [{lhs : Goal} {rhs : Goal}] #:transparent)
(struct Conj2 [{lhs : Goal} {rhs : Goal}] #:transparent)
(struct Call-with-state [{fn : (-> State Stream)}] #:transparent)
(struct == [{lhs : Term} {rhs : Term}] #:transparent)

(struct Bind [{stream : Stream} {goal : Goal}] #:transparent)
(struct Mplus [{lhs : Stream} {rhs : Stream}] #:transparent)
(struct Pause [{state : State} {goal : Goal}] #:transparent)

(: step-goal : State Goal -> Stream)
(define (step-goal state goal)
  (match goal
    [(Disj2 lhs rhs) (Mplus (Pause state lhs)
                            (Pause state rhs))]
    [(Conj2 lhs rhs) (Bind (Pause state lhs)
                           rhs)]
    [(== lhs rhs) 
     (match-define (State subst) state)
     (let ([subst* (unify lhs rhs subst)])
       (if subst*
         (cons (State subst*) '())
         '()))]
    [(Call-with-state f)
     (f state)]))

(: step-stream : Stream -> Stream)
(define (step-stream stream)
  (match stream
    [(Bind s g) 
     (match s 
       ['() '()]
       [(cons fst '())
        (Pause fst g)]
       [(cons fst snd)
        (Mplus (Pause fst g)
               (Bind snd g))]
       [(Bind s* g*)
        (Bind s* (Conj2 g* g))]
       [_ (Bind (step-stream s) g)])]
    [(Mplus lhs rhs) 
       (match lhs
         ['() rhs]
         [(cons lhs-fst lhs-snd)
          (cons lhs-fst (Mplus rhs lhs-snd))]
         [_ (Mplus rhs (step-stream lhs))])]
    [(Pause state goal) (step-goal state goal)]
    [(or (? null?) 
         (? pair?)) 
     stream]))

(define fail (== 0 1))

(define-syntax fresh
  (syntax-rules ()
    [(_ (x ...) g0 g ...)
     (Call-with-state
       (lambda ({state : State})
         (define sub (State-subst state))
         (let*-values ([{x sub} (subst-new-var sub)] 
                       ...) 
           (Pause (State sub) (conj g0 g ...)))))]))

(define-syntax conj
  (syntax-rules ()
    [(_ g) g]
    [(_ g0 g ...) (Conj2 g0 (conj g ...))]))

(define-syntax disj
  (syntax-rules ()
    [(_ g) g]
    [(_ g0 g ...) (Disj2 g0 (disj g ...))]))

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

(: stream->list : Stream -> (Listof State))
(define (stream->list s)
  (match s
    ['() '()]
    [(cons fst snd) (cons fst (stream->list snd))]
    [_ (stream->list (step-stream s))]))

(: reify : Term State -> Term)
(define (reify term state) 
  (let ([subst (State-subst state)])
    (walk* term subst)))

(define-syntax run*
  (syntax-rules ()
    [(_ (x ...) g0 g ...)
     (run* q (fresh (x ...) 
               (== q (list x ...)) 
               g0 g ...))]
    [(_ q g0 g ...)
     (let* ([goal : Goal (fresh (q) g0 g ...)]
            [stream : Stream (step-goal *initial-state* goal)])
       (let ([q (var 0)])
         (map (lambda ({s : State}) : Term (reify q s)) 
              (stream->list stream))))]))

(module+ test
  (require typed/rackunit)
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

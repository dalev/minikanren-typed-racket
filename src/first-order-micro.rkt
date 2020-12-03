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

(define-type (FK o) (-> o))
(define-type (SK o a) (-> a (FK o) o))
(define-type (T a)
  (All (o) (-> (SK o a) (FK o) o)))

(: unit : (All (a) (-> a (T a))))
(define (unit x)
  (lambda (k f) 
    (k x f)))

(: bind : (All (a b) (-> (T a) (-> a (T b)) (T b))))
(define #:forall (a b) bind 
  (lambda ({t : (T a)} g)
    (lambda #:forall (o) ({k : (SK o b)} {f : (FK o)})
      (t (lambda ({x : a} {f : (FK o)}) 
           ((g x) k f))
         f))))

(: mzero : (All (a) (T a)))
(define mzero (lambda (_k f) (f)))

(: mplus : (All (a) (-> (T a) (T a) (T a))))
(define (mplus lhs rhs)
  (lambda (k f)
    (lhs k (lambda () (rhs k f)))))

(: msplit : (All (a) (-> (T a) (T (Pair a (T a))))))
(define msplit 
  (lambda #:forall (a) ({t : (T a)})
    (define-type Obs (T (Pair a (T a))))
    (: ssk : (SK Obs a))
    (define (ssk v fk)
      (lambda (a b)
        (a (cons
            v 
            (lambda #:forall (o) ({s* : (SK o a)} {f* : (FK o)})
              ((fk)
               (lambda ({pair : (Pair a (T a))} _) 
                 (match-define (cons v** x) pair)
                 (s* v** 
                     (lambda () (x s* f*))))
               f*)))
           (lambda () (b)))))
    (: ffk : (FK Obs))
    (define (ffk) (lambda (_ f) (f)))
    (t ssk ffk)))

(: interleave : (All (a) (T a) (T a) -> (T a)))
(define (interleave lhs rhs)
  ((inst bind (Pair a (T a)) a)
   ((inst msplit a) lhs)
   (lambda ({pair : (Pair a (T a))})
     (match-define (cons lhs-fst lhs-rest) pair)
     ((inst mplus a) 
      ((inst unit a) lhs-fst)
      ((inst interleave a) rhs lhs-rest)))))

(define-type Goal (U Disj2 Conj2 == Call-with-state))

(define-type Stream (U Bind Mplus Pause
                       Null
                       (Pair State Stream)))

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

(: step-stream-cps : (All (t) (-> Stream t) Stream -> t))
(define (step-stream-cps k stream)
  (match stream
    [(Bind s g) 
     (match s 
       ['() (k '())]
       [(cons fst '())
        (k (Pause fst g))]
       [(cons fst snd)
        (k (Mplus (Pause fst g)
                  (Bind snd g)))]
       [_ (step-stream-cps (lambda ({v : Stream}) (k (Bind v g))) s)])]
    [(Mplus lhs rhs) 
       (match lhs
         ['() (k rhs)]
         [(cons lhs-fst '())
          (k (cons lhs-fst rhs))]
         [(cons lhs-fst lhs-snd)
          (k (cons lhs-fst (Mplus rhs lhs-snd)))]
         [_ 
           (step-stream-cps (lambda ({lhs* : Stream}) (k (Mplus rhs lhs*))) lhs)])]
    [(Pause state goal) (k (step-goal state goal))]
    [(or (? null?) 
         (? pair?)) 
     (k stream)]))

(define-type Context (U Init-k Bind-k Mplus-k))
(struct Init-k [] #:transparent)
(struct Bind-k [{ctx : Context} {goal : Goal}] #:transparent)
(struct Mplus-k [{ctx : Context} {rhs : Stream}] #:transparent)

(: apply-context : Context Stream -> Stream)
(define (apply-context k s)
  (match k
    [(Init-k) s]
    [(Bind-k k* goal) 
     (apply-context k* (Bind s goal))]
    [(Mplus-k k* rhs) 
     (apply-context k* (Mplus rhs s))]))

(: step-stream : (Context Stream -> Stream))
(define (step-stream k stream)
  (match stream
    [(Bind s g) 
     (match s 
       ['() (apply-context k '())]
       [(cons fst '())
        (apply-context k (Pause fst g))]
       [(cons fst snd)
        (apply-context k (Mplus (Pause fst g)
                                (Bind snd g)))]
       [_ (step-stream
            (Bind-k k g)
            s)])]
    [(Mplus lhs rhs) 
       (match lhs
         ['() (apply-context k rhs)]
         [(cons lhs-fst '())
          (apply-context k (cons lhs-fst rhs))]
         [(cons lhs-fst lhs-snd)
          (apply-context k (cons lhs-fst (Mplus rhs lhs-snd)))]
         [_ 
           (step-stream
             (Mplus-k k rhs)
             lhs)])]
    [(Pause state goal) 
     (apply-context k (step-goal state goal))]
    [(or (? null?) 
         (? pair?)) 
     (apply-context k stream)]))

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
    [_ (stream->list (step-stream (Init-k) s))]))

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

#lang typed/racket/base

(require racket/match 
         (prefix-in rt: "term.rkt"))

(struct State [{subst : rt:Subst}] #:transparent)

(define *initial-state* (State rt:subst-empty))

(define-type Goal (U Disj2 Conj2 == Fresh Fail))

(define-type Stream (U Bind Mplus Pause
                       Null
                       (Pair State Stream)))

(: stream-ready? : Stream -> Boolean)
(define (stream-ready? s)
  (match s
    ['() #t]
    [(cons _1 _2) #t]
    [_ #f]))

(struct Relation [{name : Symbol} {params : (Listof Svar)} {body : Goal}] #:transparent)
(define-type Env (Immutable-HashTable Symbol Relation))
(define env-empty (hasheqv))

(define-type Term (U Svar Number Symbol Null (Pair Term Term)))

(struct Fail [] #:transparent)
(struct Svar [{name : Symbol}] #:transparent)
(struct Disj2 [{lhs : Goal} {rhs : Goal}] #:transparent)
(struct Conj2 [{lhs : Goal} {rhs : Goal}] #:transparent)
(struct Fresh [{vars : (Listof Svar)} {body : Goal}] #:transparent)
(struct == [{lhs : Term} {rhs : Term}] #:transparent)
(struct Call [{rator : Symbol} {rands : (Listof Term)}] #:transparent)

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
     (let ([subst* (rt:unify lhs rhs subst)])
       (if subst*
         (cons (State subst*) '())
         '()))]))

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

(module+ test
  (require typed/rackunit)
  (define env 
    (hasheq 'appendo 
            (Relation 
              'appendo
              (map Svar (list 'xs 'ys 'zs))
              (Disj2 (Conj2 (Call 'nullo (list (Svar 'xs)))
                            (== (Svar 'ys) (Svar 'zs)))
                     (Fresh (map Svar (list 'x 'xs* 'suffix))
                            (Conj2 (== (cons (Svar 'x) (Svar 'xs*)) (Svar 'xs))
                                   (Conj2 (== (cons (Svar 'x) (Svar 'suffix)) (Svar 'zs))
                                          (Call 'appendo
                                                (map Svar (list 'xs* 'ys 'suffix))))))))))

  )



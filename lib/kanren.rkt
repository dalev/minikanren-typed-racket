#lang racket/base
(require
  (prefix-in subst: "skew-bral.rkt")
  racket/function
  racket/set
  racket/generator
  racket/sequence
  (except-in racket/match ==)
  (for-syntax racket/base
              syntax/parse))
(provide
  == ==-check 
  conde
  all
  any
  fail succeed
  fresh run run* in-solutions
  project

  ;; committed choice stuff
  once!
  ifa conda
  ifu condu
  )

(module+ test (require (except-in rackunit fail)))

(define var? subst:var?)

(define-syntax lambdag@
  (syntax-rules ()
    ((_ (s) e) (lambda (s) e))))

(define (both is? a b) (and (is? a) (is? b)))

(define (in-compound-struct s)
  (define-values (stype _) (struct-info s))
  (define-values (name init-field-cnt auto-field-cnt accessor-proc mutator-proc immutable-k-list super-type skipped?) (struct-type-info stype))
  (define total-field-cnt (+ init-field-cnt)
    #;(compound-struct-type-field-cnt stype))
  (sequence-map (curry accessor-proc s) (in-range total-field-cnt)))

(define (compound-struct-map f s)
  (define-values (stype _) (struct-info s))
  (define make (struct-type-make-constructor stype))
  (apply make 
         (for/list ([e (in-compound-struct s)])
           (f e))))

(define (compound-struct-same? x y)
  (define-values (xtype _) (struct-info x))
  ((struct-type-make-predicate xtype) y))  

(define (compound-struct-cmp x y field=?)
  (and (compound-struct-same? x y)
       (for/and ([ex (in-compound-struct x)]
                 [ey (in-compound-struct y)])
         (field=? ex ey))))

(define (compound-struct? v)
  (let-values ([(stype skipped?) (struct-info v)])
    (and stype (not skipped?))))

(define-syntax-rule (or* x f ...) (or (f x) ...))

(define (unify #:occurs-check? [occurs-check? #f] s v^ w^)
  (define extend (if occurs-check? ext-s-check subst:extend))
  (let loop ([v^ v^]
             [w^ w^]
             [s s])
    (and s
         (let ()
           (define v (subst:walk v^ s))
           (define w (subst:walk w^ s))
           (cond
             [(eq? v w) s]
             [(and (both var? v w)
                   (subst:var=? v w))
              s]
             [(var? v) (extend s v w)]
             [(var? w) (extend s w v)]
             [(both pair? v w)
              (let ([s (loop (car v) (car w) s)])
                (loop (cdr v) (cdr w) s))]
             [(and (both vector? v w)
                   (= (vector-length v) (vector-length w)))
              (for/fold ([s s]) ([a (in-vector v)]
                                 [b (in-vector w)])
                (loop a b s))]
             [(and (both compound-struct? v w)
                   (compound-struct-same? v w))
              (for/fold ([s s]) ([a (in-compound-struct v)]
                                 [b (in-compound-struct w)])
                (loop a b s))]
             [(both mpair? v w)
              (let ([s (loop (mcar v) (mcar w) s)])
                (loop (mcdr v) (mcdr w) s))]
             [(both box? v w)
              (loop (unbox v) (unbox w) s)]
             [(equal? v w) s]
             [else #f])))))

(define (occurs-check s x v)
  (let ([v (subst:walk v s)])
    (cond
      [(var? v) (eq? v x)]
      [(pair? v)
       (or (occurs-check s x (car v))
           (occurs-check s x (cdr v)))]
      [(vector? v)
       (for/or ([a (in-vector v)])
         (occurs-check s x a))]
      [(compound-struct? v)
       (for/or ([a (in-compound-struct v)])
         (occurs-check s x a))]
      [(mpair? v)
       (or (occurs-check s x (mcar v))
           (occurs-check s x (mcdr v)))]
      [(box? v) (occurs-check s x (unbox v))]
      [else #f])))

(define (walk* w s)
  (let ([v (subst:walk w s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons
         (walk* (car v) s)
         (walk* (cdr v) s))]
      [(vector? v)
       (for/vector #:length (vector-length v)
                   ([a (in-vector v)])
          (walk* a s))]
      [(compound-struct? v)
       (compound-struct-map (λ (a) (walk* a s)) v)]
      [(box? v) (box (walk* (unbox v) s))]
      [(mpair? v)
       (mcons (walk* (mcar v) s)
              (walk* (mcdr v) s))]
      [else v])))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define (reify v s)
  (define table (make-hasheq))
  (define count -1)
  (let loop ([v v])
    (let ([v (subst:walk v s)])
      (cond
        [(var? v)
         (cond [(hash-ref table v #f) => (λ (x) x)]
               [else
                 (set! count (+ 1 count))
                 (define name (reify-name count))
                 (hash-set! table v name)
                 name])]
        [(pair? v) (cons (loop (car v)) (loop (cdr v)))]
        [(vector? v)
         (for/vector #:length (vector-length v)
                     ([x (in-vector v)])
                     (loop x))]
        [(compound-struct? v)
         (compound-struct-map loop v)]
        [(box? v) (box (loop (unbox v)))]
        [(mpair? v) (mcons (loop (mcar v)) (loop (mcdr v)))]
        [else v]))))

(define-syntax (project stx)
  (syntax-parse stx
    [(_ (x:id ...) goal:expr g:expr ...)
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(x ...)))
                   "duplicate variable name"
     #'(lambdag@ (s)
                 (let ([x (walk* x s)] ...)
                   (bind* (goal s) g ...)))]))

(define-syntax (fresh stx)
  (syntax-parse stx
    [(_ (x:id ...) goal:expr g:expr ...)
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(x ...)))
                   "duplicate variable name"
      #'(λ (s)
           (let*-values ([(x s) (subst:create-variable 'x s)] ...)
             (bind* (goal s) g ...)))]))

(define-syntax (stream-case stx)
  (syntax-parse stx
                #:literals (empty singleton choice incomplete)
    [(_ stream
        [empty empty-result:expr] 
        [(singleton s:id) singleton-result:expr] 
        [(choice a:id f-choice:id) choice-result:expr]
        [(incomplete f-incomplete:id) incomplete-result:expr])
     #'(match stream
         [#f empty-result]
         [(choice a f-choice) choice-result]
         [(and (? procedure?) f-incomplete) incomplete-result]
         [s singleton-result])]))

(define (take n f)
  (if (and n (zero? n))
    '()
    (stream-case (f)
      [empty '()]
      [(singleton a) (list a)]
      [(choice a f) (cons a (take (and n (- n 1)) f))]
      [(incomplete f) (take n f)])))

(define-syntax (in-solutions stx)
  (syntax-parse stx
    [(_ (x:id) goal ...+)
     #'(in-generator
         (let loop ([s (λ () 
                         ((fresh (x) goal ... (λ (a) (reify x a)))
                          subst:empty))])
           (stream-case (s)
              [empty '()]
              [(singleton a) (yield a)]
              [(choice a f) (begin (yield a) (loop f))]
              [(incomplete f) (loop f)])))]))

(define-syntax (run stx)
  (syntax-parse stx
    [(_ n (x:id ...) goal:expr ...+)
     #'(take n
             (thunk
               ((fresh (x ...) goal ... (λ (a) (list (reify x a) ...)))
                subst:empty)))]))

(define-syntax (run* stx)
  (syntax-parse stx
    [(_ (x:id ...) g:expr ...+) 
     #'(run #f (x ...) g ...)]))

(define (ext-s-check s x v)
  (cond
    [(occurs-check s x v) #f]
    [else (subst:extend s x v)]))

(define singleton 'singleton)
(define incomplete 'incomplete)
(define empty '())

(define mzero #f)

(struct choice (first rest))

(define (bind stream g)
  (stream-case stream
    [empty mzero]
    [(singleton a) (g a)]
    [(choice a f) 
     (mplus (g a) (thunk (bind (f) g)))]
    [(incomplete f) (thunk (bind (f) g))]))

(define-syntax (bind* stx)
  (syntax-parse stx
    [(_ e) #'e]
    [(_ e g0 g ...)
     #'(bind* (bind e g0) g ...)]))

;; CR dalev: consider whether [all] wants to use this
(define (bind-in-order stream g)
  (stream-case stream
    [empty mzero]
    [(singleton a) (g a)]
    [(choice a f)
     (mappend (g a) (thunk (bind (f) g)))]
    [(incomplete f) (thunk (bind (f) g))]))

(define (mappend stream f)
  (stream-case stream
    [empty (f)]
    [(singleton a) (choice a f)]
    [(choice a f0)
     (choice a (thunk (mappend (f0) f)))]
    [(incomplete f0)
     (thunk (mappend (f0) f))]))

(define-syntax (bind-in-order* stx)
  (syntax-parse stx
    [(_ e) #'e]
    [(_ e g0 g ...)
     #'(bind-in-order* (bind-in-order e g0) g ...)]))

(define (mplus a-inf f)
  (stream-case a-inf
    [empty (f)] 
    [(singleton a) (choice a f)]
    [(choice a f0) 
     ;; interleaving
     (choice a (thunk (mplus (f) f0)))]
    [(incomplete f0)
     (thunk (mplus (f) f0))]))

(define-syntax (mplus* stx)
  (syntax-parse stx
    [(_ e) #'e]
    [(_ e0 e ...)
     #'(mplus e0 (thunk (mplus* e ...)))]))

(define (== v w)
  (lambda (s)
    (or (unify s #:occurs-check? #f v w)
        mzero)))

(define succeed (== #t #t))
(define fail (== #t #f))

(define (==-check v w)
  (lambda (s)
    (or (unify s #:occurs-check? #t v w)
        mzero)))

(define-syntax (all stx)
  (syntax-parse stx
    [(_) #'succeed]
    [(_ g) #'g]
    [(_ g0 g ...) #'(lambda (s) (bind* (g0 s) g ...))]))

(module+ test
  (check-equal?
    (run* (x y)
      (all (any (== x 1) (== x 2))
           (any (== y 3) fail (== y 4) (== y 5))))
    (list '(1 3) '(2 3) 
          '(1 4) '(2 4) 
          '(1 5) '(2 5))))

(define-syntax (conde stx)
  (syntax-parse stx
    [(_ (g0 g ...) (g1 g^ ...) ...)
     #'(lambda (s)
         (thunk 
           (mplus* (bind* (g0 s) g ...)
                   (bind* (g1 s) g^ ...)
                   ...)))]))

(define-syntax (any stx)
  (syntax-parse stx
    [(_ g1 g2 ...) 
     #'(lambda (s) 
         (mplus* (g1 s) (g2 s) ...))]))

#|
if g0 succeeds, then throw away g2 and solve g1
if g0 fails, then throw away g1 and solve with g2
|#
(define (ifa g0 g1 g2)
  (lambda (s)
    (let loop ([g0-solutions (g0 s)])
      (stream-case g0-solutions
        [empty (g2 s)]
        [(singleton a) (g1 a)]
        [(choice _ _)
         (bind g0-solutions g1)]
        [(incomplete f)
         (thunk (loop (f)))]))))

(module+ test

  (check-equal?
    (run* (x)
      (fresh (y)
        (ifa (any (== 3 4)
                  (== 'a 'b))
             (== x 'should-not-get-here)
             (== x 'only-solution))))
    (list '(only-solution)))

  (check-equal?
    (run* (x)
      (fresh (y)
        (ifa (any (== y 2)
                  (== y 4))
             (== x y)
             (== x 'should-not-get-here))))
    (list '(2) '(4))))

(define-syntax (conda stx)
  (syntax-parse stx 
    [(_ (g0 g ...)) #'(all g0 g ...)]
    [(_ (g0 g ...) (g1 g^ ...) ...)
     #'(ifa g0 
            (all g ...)
            (conda (g1 g^ ...) ...))]))

(define (once! g)
  (lambda (s)
    (let loop ([solutions (g s)])
      (stream-case solutions
        [empty mzero]
        [(singleton s) s]
        [(choice a _) a]
        [(incomplete f)
         (thunk (loop (f)))]))))

(define (ifu g0 g1 g2)
  (ifa (once! g0) g1 g2))

(define-syntax (condu stx)
  (syntax-parse stx
    [(_ (g0 g ...))
     #'(ifu g0 (all g ...) fail)]
    [(_ (g0 g ...) (g1 g^ ...) ...)
     #'(ifu g0 
            (all g ...)
            (condu (g1 g^ ...) ...))]))

(module+ test
  (check-equal?
    (run* (x)
      (fresh (y)
        (condu 
          [fail (== x 'impossible)]
          [(any (== y 17)
                (== y 42))
           (any (== x y)
                (== y x))]
          [(== x 34) succeed])))
    ;; second condu question succeeds once, rhs succeeds twice
    (list '(17) '(17))))


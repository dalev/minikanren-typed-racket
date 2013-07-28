#lang racket/base
(require
  (prefix-in subst: "skew-bral.rkt")
  racket/function
  racket/set
  racket/generator
  (for-syntax racket/base
              syntax/parse))
(provide
  == ==-check 
  conde condi conda condu
  all alli
  anye anyi
  fail succeed
  fresh run run* in-solutions
  project
  )

(define var? subst:var?)

(define-syntax lambdag@
  (syntax-rules ()
    ((_ (s) e) (lambda (s) e))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define (both is? a b) (and (is? a) (is? b)))

(require unstable/sequence)
(define (in-compound-struct s)
  (define-values (stype _) (struct-info s))
  (define-values (name init-field-cnt auto-field-cnt accessor-proc mutator-proc immutable-k-list super-type skipped?) (struct-type-info stype))
  (define total-field-cnt (+ init-field-cnt)
    #;(compound-struct-type-field-cnt stype))
  (sequence-lift (curry accessor-proc s) (in-range total-field-cnt)))

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

(define (atomic-struct? v) (not (compound-struct? v)))

(define (compound-struct? v)
  (let-values ([(stype skipped?) (struct-info v)])
    (and stype (not skipped?))))

(define-syntax-rule (or* x f ...) (or (f x) ...))

(define (atom? x)
  (or* x 
       string? bytes? set?
       regexp? pregexp? byte-regexp? byte-pregexp?
       number?
       boolean? char? symbol?
       keyword? null? procedure? void? 
       atomic-struct?))

(define (compound? x)
  (or* x pair? vector? mpair? box? hash? compound-struct?))

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
    [(_ (x:id ...) goal:expr ...+)
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(x ...)))
                   "duplicate variable name"
     #'(lambdag@ (s)
                 (let ([x (walk* x s)] ...)
                   ((all goal ...) s)))]))

(define-syntax (fresh stx)
  (syntax-parse stx
    [(_ (x:id ...) goal:expr ...+)
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(x ...)))
                   "duplicate variable name"
      #'(λ (s)
          (let*-values ([(x s) (subst:create-variable 'x s)] ...)
            ((all goal ...) s)))]))

(define (take n f)
  (if (and n (zero? n))
    '()
    (case-inf (f)
              '()
              [(a) (list a)]
              [(a f)
               (cons a
                     (take (and n (- n 1)) f))])))

(define-syntax (in-solutions stx)
  (syntax-parse stx
    [(_ (x:id) goal ...+)
     #'(in-generator
         (let loop ([s (λ () 
                         ((fresh (x) goal ... (λ (a) (reify x a)))
                          subst:empty))])
           (case-inf (s)
              '()
              [(a) (yield a)]
              [(a f) (begin (yield a) (loop f))])))]))

(define-syntax (run stx)
  (syntax-parse stx
    [(_ n (x:id) goal:expr ...+)
     #'(take n
             (λ ()
               ((fresh (x) goal ... (λ (a) (list (reify x a))))
                subst:empty)))]))

(define-syntax (run* stx)
  (syntax-parse stx
    [(_ (x:id) g:expr ...+) 
     #'(run #f (x) g ...)]))

(define (ext-s-check s x v)
  (cond
    [(occurs-check s x v) #f]
    [else (subst:extend s x v)]))

(define-syntax case-inf
  (syntax-rules ()
    [(_ e on-zero ((a^) on-one) ((a f) on-choice))
     (let ([a-inf e])
       (cond
         [(not a-inf) on-zero]
         [(not (and 
                 (pair? a-inf)
                 (procedure? (cdr a-inf))))
          (let ([a^ a-inf])
            on-one)]
         [else (let ([a (car a-inf)]
                     [f (cdr a-inf)])
                 on-choice)]))]))

(define-syntax mzero
  (syntax-rules ()
    ((_) #f)))

(define-syntax unit
  (syntax-rules ()
    ((_ a) a)))

(define-syntax choice 
  (syntax-rules ()
    ((_ a f) (cons a f))))

(define (== v w)
  (λ (s)
    (cond
      [(unify s #:occurs-check? #f v w) => succeed]
      [else (fail s)])))

(define (==-check v w)
  (λ (s)
    (cond
      [(unify s #:occurs-check? #t v w) => succeed]
      [else (fail s)])))

(define-syntax (all stx)
  (syntax-parse stx
    [(_) #'succeed]
    [(_ g) #'g]
    [(_ g^ g ...) #'(lambdag@ (s) (bind (g^ s) (all g ...)))]))

(define-syntax conde
  (syntax-rules (else)
    [(_) fail]
    [(_ (else g0 g ...)) (all g0 g ...)]
    [(_ (g0 g ...) c ...)
     (anye (all g0 g ...) (conde c ...))]))

(define (succeed s) (unit s))

(define (fail _s) (mzero))

(define (bind a-inf g)
  (case-inf a-inf
    (mzero) 
    ((a) (g a))
    ((a f) (mplus (g a)
              (lambdaf@ () (bind (f) g))))))

(define (mplus a-inf f)
  (case-inf a-inf
    (f) 
    ((a) (choice a f))
    ((a f0) (choice a 
              (lambdaf@ () (mplus (f0) f))))))

(define-syntax (anye stx)
  (syntax-parse stx
    [(_ g1 g2) 
     #'(lambdag@ (s)
         (mplus (g1 s) 
           (lambdaf@ () (g2 s))))]))

(define-syntax (alli stx)
  (syntax-parse stx
    [(_) #'succeed]
    [(_ g) #'g]
    [(_ g^ g ...) 
     #'(lambdag@ (s) 
         (bindi (g^ s) (alli g ...)))]))

(define-syntax condi
  (syntax-rules (else)
    [(_) fail]
    [(_ (else g0 g ...)) (all g0 g ...)]
    [(_ (g0 g ...) c ...)
     (anyi (all g0 g ...) (condi c ...))]))

(define-syntax (anyi stx)
  (syntax-parse stx
    [(_ g1 g2) 
     #'(lambdag@ (s) 
         (mplusi (g1 s) 
                 (lambdaf@ () (g2 s))))]))

(define (bindi a-inf g)
  (case-inf a-inf
    (mzero)
    ((a) (g a))
    ((a f) (mplusi (g a) 
              (lambdaf@ () (bindi (f) g))))))

(define (mplusi a-inf f)
  (case-inf a-inf
    (f) 
    ((a) (choice a f))
    ((a f0) (choice a 
                    (lambdaf@ () (mplusi (f) f0))))))

(define-syntax conda
  (syntax-rules (else)
    [(_) fail]
    [(_ (else g0 g ...)) (all g0 g ...)]
    [(_ (g0 g ...) c ...)
     (ifa g0 (all g ...) (conda c ...))]))

(define-syntax condu
  (syntax-rules (else)
    [(_) fail]
    [(_ (else g0 g ...)) (all g0 g ...)]
    [(_ (g0 g ...) c ...)
     (ifu g0 (all g ...) (condu c ...))]))

(define-syntax ifa
  (syntax-rules ()
    [(_ g0 g1 g2)
     (lambdag@ (s)
       (let ([s-inf (g0 s)] (g^ g1))
         (case-inf s-inf
           (g2 s)
           ((s) (g^ s))
           ((s f) (bind s-inf g^)))))]))

(define-syntax ifu
  (syntax-rules ()
    [(_ g0 g1 g2)
     (lambdag@ (s)
       (let ([s-inf (g0 s)] [g^ g1])
         (case-inf s-inf
           (g2 s)
           ((s) (g^ s))
           ((s f) (g^ s)))))]))

#lang racket/base
(require
  "skew-bral.rkt"
  racket/function
  racket/set
  racket/generator
  racket/sequence
  (except-in racket/match ==)
  (for-syntax racket/base
              syntax/parse))
(provide
  == ==-check =/=
  conde
  all all:l->r
  any
  fail succeed
  fresh run run* in-solutions
  project

  ;; committed choice stuff
  once!
  ifa conda
  ifu condu

  ;; experimental
  any:l->r

  ;; for Reasoned Schemer tests
  condi alli
  cond:l->r
  )

(module+ test (require (except-in rackunit fail)))

;; A substitution is just a sbral that may contain logic variables
(struct var (name index))
(define (var=? v w) (= (var-index v) (var-index w)))

(define subst:empty sbral-empty)
(define (subst:size subst) (sbral-size subst))

(define (subst:create-variable name subst)
  (define variable (var name (subst:size subst)))
  (values variable (sbral-cons variable subst)))

(define (subst:extend subst variable value)
  (sbral-set subst (var-index variable) value))

;; CR dalev: unbox the constraint structure?
(struct context (substitution constraints))
(define context:empty (context subst:empty '()))

;; Transitive closure of [sbral-ref] by chasing variable chains to the end
(define (subst:walk v sbral)
  (if (var? v)
    (match (sbral-ref sbral (var-index v)) 
      [#f #f]
      [answer
        (cond [(eq? v answer) v]
              [(and (var? answer) (var=? v answer)) v]
              [else (subst:walk answer sbral)])])
    v))

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

(define (unify #:occurs-check? occurs-check? s v^ w^)
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
             [(and (both var? v w) (var=? v w)) s]
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
       (compound-struct-map (lambda (a) (walk* a s)) v)]
      [(box? v) (box (walk* (unbox v) s))]
      [(mpair? v)
       (mcons (walk* (mcar v) s)
              (walk* (mcdr v) s))]
      [else v])))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

;; CR dalev: reify needs to say something about constraints
(define (reify v s)
  (define table (make-hasheq))
  (define count -1)
  (let loop ([v v])
    (let ([v (subst:walk v s)])
      (cond
        [(var? v)
         (cond [(hash-ref table v #f) => identity]
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
     #'(lambda (ctx)
         (define s (context-substitution ctx))
         (let ([x (walk* x s)] ...)
           (bind* (goal ctx) g ...)))]))

(define-syntax (fresh stx)
  (syntax-parse stx
    [(_ (x:id ...) goal:expr g:expr ...)
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(x ...)))
                   "duplicate variable name"
      #'(lambda (ctx)
          (define s (context-substitution ctx))
           (let*-values ([(x s) (subst:create-variable 'x s)] ...)
             (define ctx^ (context s (context-constraints ctx)))
             (bind* (goal ctx^) g ...)))]))

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
         (let loop ([s (thunk
                         ((fresh (x) 
                            goal ... 
                            (lambda (ctx) 
                              (reify x (context-substitution ctx))))
                          context:empty))])
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
               ((fresh (x ...) 
                  goal ... 
                  (lambda (ctx) (list (reify x (context-substitution ctx)) 
                                      ...)))
                context:empty)))]))

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

(define (bind-in-order stream g)
  (stream-case stream
    [empty mzero]
    [(singleton a) (g a)]
    [(choice a f)
     (mappend (g a) (thunk (bind-in-order (f) g)))]
    [(incomplete f) (thunk (bind-in-order (f) g))]))

(define (mappend stream f)
  (stream-case stream
    [empty (f)]
    [(singleton a) (choice a f)]
    [(choice a f0)
     (choice a (thunk (mappend (f0) f)))]
    [(incomplete f0)
     (thunk (mappend (f0) f))]))

(define-syntax (mappend* stx)
  (syntax-parse stx
    [(_ e) #'e]
    [(_ e e1 e2 ...)
     #'(mappend* (mappend e e1) e2 ...)]))

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
  (lambda (ctx)
    (match-define (context subst constraints) ctx)
    (match (unify subst #:occurs-check? #f v w)
      [#f #f]
      [subst*
        (if (eq? subst subst*)
          ctx
          (let ([d (subst-diff subst* subst)])
            (constrain-new-equations d (context subst* constraints))))])))

(define (constrain-new-equations equations ctx)
  (define variables (equations-lhss equations))
  (define constraints (context-constraints ctx))
  ;; CR dalev: ...
  (define (intersection-not-empty? a b) #t)
  (let loop ([constraints constraints]
             [ctx ctx])
    (match constraints
      ['() ctx]
      [(cons (disunify-constraint disunify-goal eqns) rest-cs)
       ;; CR dalev: we may also need to look at rhss for used variables
       (if (intersection-not-empty? variables (equations-lhss eqns))
         ;; CR dalev: GC exhausted constraints
         (bind (disunify-goal ctx)
               (lambda (ctx^)
                 (loop rest-cs ctx^)))
         (loop rest-cs ctx))])))

(define succeed (== #t #t))
(define fail (== #t #f))

(define (==-check v w)
  (lambda (ctx)
    (match (unify (context-substitution ctx) #:occurs-check? #t v w)
      [#f #f]
      [subst (context subst (context-constraints ctx))])))

(struct equations (lhss rhss))
(define (equations-empty? d) (null? (equations-lhss d)))

;; CR dalev: hack the unifier to accumulate the diff instead
(define (subst-diff s t)
  ;; in practice, we are only interested in diffs where s results from
  ;; unifying some terms using t -- it cannot shrink.
  (unless (>= (subst:size s) (subst:size t))
    (error 'subst-diff "expected bigger substitution on the left: ~a ~a"
           (subst:size s) (subst:size t)))
  (define-values {lhss rhss}
    (for/fold ([lhss '()] [rhss '()]) ([i (in-range (subst:size s))])
      (define s@i (sbral-ref s i))
      (if (>= i (subst:size t))
        (values (cons (var 'dummy i) lhss)
                (cons s@i rhss))
        (let ([t@i (sbral-ref t i)])
          ;; CR dalev: does eq? suffice since we assume s extends t?
          (if (equal? s@i t@i)
            (values lhss rhss)
            (values (cons (var 'dummy i) lhss)
                    (cons s@i rhss)))))))
  (equations lhss rhss))

;; Produce a goal 
(define (=/= u v)
  (lambda (ctx) (constrain-=/= ctx u v)))

(module+ test

  (define (%rember/broken xs target out)
    (conde 
      [(== xs '()) (== '() out)]
      [else
        (fresh (first-xs rest-xs res)
          (== (cons first-xs rest-xs) xs)
          (%rember/broken rest-xs target res)
          (conde
            [(== target first-xs) (== res out)]
            [else (== (cons first-xs res) out)]))]))

  (check-equal?
    (run* (q) (%rember/broken '(a b a c) 'a q))
    (map list '((b c) (b a c) (a b c) (a b a c))))

  (define (%rember xs target out)
    (conde 
      [(== xs '()) (== '() out)]
      [else
        (fresh (first-xs rest-xs res)
          (== xs (cons first-xs rest-xs))
          (conde
            [(== target first-xs)
             (%rember rest-xs target out)]
            [(=/= target first-xs)
             (%rember rest-xs target res)
             (== (cons first-xs res) out)]))]))

  (check-equal?
    (run* (q) (%rember '(a b a c) 'a q))
    (map list '((b c))))
  )



;; Add constraints to [ctx] that ensure [u] and [v] cannot unify
(define (constrain-=/= ctx u v)
  (define s (context-substitution ctx))
  (match (unify s #:occurs-check? #f u v)
    [#f ctx] ;; u and v do not unify -- success
    [s^ (=/=-goal ctx (subst-diff s^ s))]))

(define (=/=-goal ctx eqns)
  (match-define (context s c) ctx)
  (match-define (equations lhss rhss) eqns)
  (match (unify s #:occurs-check? #f lhss rhss)
    [#f ctx]
    [s^
      (let ([equations^ (subst-diff s^ s)])
        (if (equations-empty? equations^)
          #f
          (normalize-store equations^ ctx)))]))

(struct disunify-constraint (goal equations))

(define (equations->substitution eqns)
  (match-define (equations lhss rhss) eqns)
  ;; CR dalev: O(largest-index) is sad
  (define subst-with-unbound-variables
    (match lhss
      [(cons (var _ largest-index) _)
       (for/fold ([subst subst:empty]) ([i (in-range (+ 1 largest-index))])
         (let-values ([{_ subst} (subst:create-variable 'dummy subst)])
           subst))]
      ['() subst:empty]))
  (unify subst-with-unbound-variables #:occurs-check? #f lhss rhss))

;; Produce #t when [eqns^] add no information to [eqns]
(define (subsumes? eqns^ eqns)
  (match-define (equations lhss^ rhss^) eqns^)
  (match (equations->substitution eqns)
    [#f (error 'subsumes? "cannot turn into subst: ~a" eqns)]
    [subst
      (match (unify subst #:occurs-check? #f lhss^ rhss^)
        [#f #f]
        [subst^ (eq? subst subst^)])]))
         
(define (normalize-store equations ctx)
  (match-define (context s constraints) ctx)
  (let loop ([constraints constraints]
             [new-constraints '()])
    (match constraints
      ['()
       (context s (cons (disunify-constraint 
                          ;; CR dalev: closure not necessary
                          (lambda (ctx)
                            (=/=-goal ctx equations))
                          equations)
                        new-constraints))]
      [(cons c rest-constraints)
       (define c-equations (disunify-constraint-equations c))
       (cond [(subsumes? c-equations equations) 
              ctx]
             [(subsumes? equations c-equations)
              ;; we do not need c anymore
              (loop rest-constraints
                    new-constraints)]
              [else (loop rest-constraints
                          (cons c new-constraints))])])))

(define-syntax (all stx)
  (syntax-parse stx
    [(_) #'succeed]
    [(_ g) #'g]
    [(_ g0 g ...) #'(lambda (c) (bind* (g0 c) g ...))]))

(define-syntax (all:l->r stx)
  (syntax-parse stx
    [(_) #'succeed]
    [(_ g) #'g]
    [(_ g0 g ...) #'(lambda (c) (bind-in-order* (g0 c) g ...))]))

(module+ test
  (define (%repeat goal)
    (conde 
      [goal succeed]
      [succeed (%repeat goal)]))

  (define n 10)

  ;; [all] does not get stuck in the first (infinite) goal:
  (check-equal? 
    (run n (x)
      (all (%repeat succeed)
           (== x 42)))
    (map list (for/list ([i (in-range 0 n)]) 42)))

  (check-equal? 
    (run n (x)
      (all (== x 42) (%repeat succeed)))
    (map list (for/list ([i (in-range 0 n)]) 42)))
  
  ;; [all:l->r] does not get stuck
  (check-equal?
    (run 10 (x)
      (all:l->r (%repeat succeed)
                (== x 42)))
    (map list (for/list ([i (in-range 0 n)]) 42)))

  (check-equal?
    (run 10 (x) (all:l->r (== x 42) (%repeat succeed)))
    (map list (for/list ([i (in-range 0 n)]) 42)))
)

(module+ test
  ;; [all] and [all:l->r] produce answers in different order
  (check-equal?
    (run* (x y)
      (all (any (== x 1) (== x 2))
           (any (== y 3) fail (== y 4) (== y 5))))
    (list '(1 3) '(2 3) 
          '(1 4) '(2 4) 
          '(1 5) '(2 5)))

  (check-equal?
    (run* (x y)
      (all:l->r (any (== x 1) (== x 2))
                       (any (== y 3) fail (== y 4) (== y 5))))
    (list '(1 3) '(1 4) '(1 5) 
          '(2 3) '(2 4) '(2 5)))
  )

(define-syntax (conde stx)
  (syntax-parse stx #:literals (else)
    [(_ (g:expr ...+) ... (else g-final:expr ...+))
     #'(conde (g ...) ... (succeed g-final ...))]
    [(_ (g0:expr g:expr ...) ...+)
     #'(lambda (ctx)
         (thunk
           (mplus* (bind* (g0 ctx) g ...)
                   ...)
           )
         )]))

(define-syntax (condi stx)
  (syntax-parse stx #:literals (else)
    [(_ (g:expr ...+) ... (else g-final:expr ...+))
     #'(condi (g ...) ... (succeed g-final ...))]
    [(_ (g0:expr g:expr ...) ...+)
     #'(lambda (ctx)
         (mplus* (bind* (g0 ctx) g ...)
                 ...))]))

(define-syntax (cond:l->r stx)
  (syntax-parse stx #:literals (else)
    [(_ (g:expr ...+) ... (else g-final:expr ...+))
     #'(cond:l->r (g ...) ... (succeed g-final ...))]
    [(_ (g0:expr g:expr ...) ...+)
     #'(lambda (s)
         (thunk 
           (mappend* (thunk (bind-in-order* (g0 s) g ...))
                     ...)))]))

;; CR dalev: ...
(define-syntax (alli stx)
  (syntax-parse stx
    [(_ goal:expr ...) #'(all goal ...)]))

(define-syntax (any stx)
  (syntax-parse stx
    [(_ g1 g2 ...) 
     #'(lambda (s)
         (mplus* (g1 s) (g2 s) ...))]))

;; CR dalev: think about this harder...
(define-syntax (any:l->r stx)
  (syntax-parse stx
    [(_ g1 g2 ...)
     #'(lambda (s)
         (mappend* (g1 s) (thunk (g2 s))...))]))

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
  (syntax-parse stx #:literals (else)
    [(_ (else g-final)) #'g-final]
    [(_ (g0 g ...)) #'(all g0 g ...)]
    [(_ (g0 g ...) (g1 g^ ...) ...)
     #'(ifa g0 
            (all g ...)
            (conda (g1 g^ ...) ...))]))

(define (once! g)
  (lambda (c)
    (let loop ([solutions (g c)])
      (stream-case solutions
        [empty mzero]
        [(singleton c) c]
        [(choice a _) a]
        [(incomplete f)
         (thunk (loop (f)))]))))

(define (ifu g0 g1 g2)
  (ifa (once! g0) g1 g2))

(define-syntax (condu stx)
  (syntax-parse stx #:literals (else)
    [(_ (else g:expr ...+))
     #'(all g ...)]
    [(_ (g0:expr g:expr ...))
     #'(ifu g0 (all g ...) fail)]
    [(_ (g0:expr g:expr ...) (g1:expr g^:expr ...) ...)
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


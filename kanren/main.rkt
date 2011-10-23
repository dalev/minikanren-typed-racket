#lang racket/base
(require
  "skew-bral.rkt"
  (for-syntax racket/base
              syntax/parse))
(provide
  == ==-check conde condi conda condu
  all alli
  fail succeed
  exist run run*
  project
  )

(define empty-s k:empty)
(define ext-s k:update)
(define size-s k:size)

(define-syntax lambdag@
  (syntax-rules ()
    ((_ (s) e) (lambda (s) e))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define-syntax rhs
  (syntax-rules ()
    [(rhs p) (cdr p)]))

(define-syntax lhs
  (syntax-rules ()
    [(lhs p) (car p)]))

(define (both is? a b) (and (is? a) (is? b)))

(define (unify #:occurs-check? [occurs-check? #f] v^ w^ s)
  (define extend (if occurs-check? ext-s-check ext-s))
  (let loop ([v^ v^]
             [w^ w^]
             [s s])
    (define v (walk v^ s))
    (define w (walk w^ s))
    (cond
      [(eq? v w) s]
      [(var? v) (extend v w s)]
      [(var? w) (extend w v s)]
      [(both pair? v w)
       (let ((s (loop (car v) (car w) s)))
         (and s (loop (cdr v) (cdr w) s)))]
      [(and (both vector? v w)
            (= (vector-length v) (vector-length w)))
       (for/fold ([s s]) ([a (in-vector v)]
                          [b (in-vector w)])
         (and s (loop a b s)))]
      [(equal? v w) s]
      [else #f])))

(define (occurs-check x v s)
  (let ([v (safe-walk v s)])
    (cond
      [(var? v) (eq? v x)]
      [(pair? v)
       (or
         (occurs-check x (car v) s)
         (occurs-check x (cdr v) s))]
      [(vector? v)
       (for/or ([a (in-vector v)])
         (occurs-check x a s))]
      [else #f])))

(define (walk* w s)
  (let ([v (safe-walk w s)])
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
      [else v])))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define (reify v s)
  (define table (make-hasheq))
  (define count -1)
  (let loop ([v v])
    (let ([v (safe-walk v s)])
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
        [else v]))))

(define-syntax project
  (syntax-rules ()
    [(_ (x ...) g ...)
     (lambdag@ (s)
               (let ([x (walk* x s)] ...)
                 ((all g ...) s)))]))

(define-syntax (exist stx)
  (syntax-parse stx
    [(_ (x ...) g0 g ...)
      #'(λ (s)
          (let*-values ([(x s) (k:new-var 'x s)] ...)
            ((all g0 g ...) s)))]))

(define (take n f)
  (if (and n (zero? n))
    '()
    (case-inf (f)
              '()
              [(a) a]
              [(a f)
               (cons (car a)
                     (take (and n (- n 1)) f))])))

(define-syntax (run stx)
  (syntax-parse stx
    [(_ n (x) g0 g ...)
     #'(take n
             (λ ()
               ((exist (x) g0 g ... (λ (a) (list (reify x a))))
                empty-s)))]))

(define-syntax run*
  (syntax-rules ()
    [(_ (x) g ...) (run #f (x) g ...)]))

;; walk based on skew binary random access lists
(define (walk v s)
  (cond
    [(var? v)
     (cond
       [(k:lookup v s) 
        => (lambda (a)
             (let [(a (k:get-value a))]
               (cond
                 [(eq? v a) v]
                 [else (walk a s)])))]
       [else v])]
    [else v]))

(define safe-walk walk)

(define (ext-s-check x v s)
  (cond
    [(occurs-check x v s) #f]
    [else (ext-s x v s)]))

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

(define == 
  (lambda (v w)
    (lambdag@ (s)
      (cond
        ((unify #:occurs-check? #f v w s) => succeed)
        (else (fail s))))))

(define ==-check
  (lambda (v w)
    (lambdag@ (s)
      (cond
        ((unify #:occurs-check? #t v w s) => succeed)
        (else (fail s))))))

(define-syntax all
  (syntax-rules ()
    ((_) succeed)
    ((_ g) (lambdag@ (s) (g s)))
    ((_ g^ g ...) (lambdag@ (s) (bind (g^ s) (all g ...))))))

(define-syntax conde
  (syntax-rules (else)
    ((_) fail)
    ((_ (else g0 g ...)) (all g0 g ...))
    ((_ (g0 g ...) c ...)
     (anye (all g0 g ...) (conde c ...)))))

(define succeed (lambdag@ (s) (unit s)))

(define fail (lambdag@ (s) (mzero)))

(define bind
  (lambda (a-inf g)
    (case-inf a-inf
      (mzero) 
      ((a) (g a))
      ((a f) (mplus (g a)
               (lambdaf@ () (bind (f) g)))))))

(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
      (f) 
      ((a) (choice a f))
      ((a f0) (choice a 
                (lambdaf@ () (mplus (f0) f)))))))

(define-syntax anye
  (syntax-rules ()
    ((_ g1 g2) 
     (lambdag@ (s)
       (mplus (g1 s) 
         (lambdaf@ () (g2 s)))))))

(define-syntax alli
  (syntax-rules ()
    ((_) succeed)
    ((_ g) (lambdag@ (s) (g s)))
    ((_ g^ g ...) 
     (lambdag@ (s) 
       (bindi (g^ s) (alli g ...))))))

(define-syntax condi
  (syntax-rules (else)
    ((_) fail)
    ((_ (else g0 g ...)) (all g0 g ...))
    ((_ (g0 g ...) c ...)
     (anyi (all g0 g ...) (condi c ...)))))

(define-syntax anyi
  (syntax-rules ()
    ((_ g1 g2) 
     (lambdag@ (s) 
       (mplusi (g1 s) 
         (lambdaf@ () (g2 s)))))))

(define bindi
  (lambda (a-inf g)
    (case-inf a-inf
      (mzero)
      ((a) (g a))
      ((a f) (mplusi (g a) 
               (lambdaf@ () (bindi (f) g)))))))

(define mplusi
  (lambda (a-inf f)
    (case-inf a-inf
      (f) 
      ((a) (choice a f))
      ((a f0) (choice a 
                (lambdaf@ () (mplusi (f) f0)))))))

(define-syntax conda
  (syntax-rules (else)
    ((_) fail)
    ((_ (else g0 g ...)) (all g0 g ...))
    ((_ (g0 g ...) c ...)
     (ifa g0 (all g ...) (conda c ...)))))

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

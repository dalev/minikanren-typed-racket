#lang racket/base
(require
  (for-syntax racket/base
              syntax/parse))
(provide
  == ==-check conde condi conda condu
  all alli
  fail succeed
  exist run run*
  project
  )

#|  Skew BRAL impl |#
(require racket/fixnum)
  (struct node (val even odd))
  (struct var (name idx))

  ; -- public 

  (define k:empty '(0))

  (define k:size
    (lambda (ls)
      (car ls)))

  (define (k:new-var name ls)
    (let ([x (var name (car ls))])
      (values x (k:associate x ls))))

  (define k:associate
    (lambda (v ls)
      (cons (fx+ 1 (car ls)) (skew-bind v (cdr ls)))))

  (define from-tail
    (lambda (i ls)
      (fx- (fx- (car ls) 1) i)))

  (define k:lookup
    (lambda (v ls)
      (lookup (from-tail (var-idx v) ls) (cdr ls))))

  (define k:update
    (lambda (a-var v ls)
      (cons (car ls) (update (from-tail (var-idx a-var) ls) v (cdr ls)))))

  (define k:get-value
    (lambda (v)
      (cond
        [(node? v) (node-val v)]
        [else (car v)])))

  ; --- helpers
  
  (define (fxzero? x) (fx= x 0))

  (define shift (lambda (n) (fxrshift n 1)))

  ;; CR dalev: horrifying code
  (define skew-bind
    (lambda (v ls)
      (cond
        [(and (pair? ls) (pair? (cdr ls)) (fx= (caar ls) (caadr ls)))
         (cons `(,(fx+ 1 (fx+ (caar ls) (caadr ls)))
                  . ,(node v (cdar ls) (cdadr ls))) (cddr ls))]
        [else (cons `(1 . ,v) ls)])))

  (define lookup-tree
    (lambda (w i t)
      (cond
        [(node? t)
         (if (fxzero? i) t ;(node-val t)
             (let [(w/2 (shift w))]
               (if (fx<= i w/2)
                   (lookup-tree w/2 (fx- i 1) (node-even t))
                   (lookup-tree w/2 (fx- (fx- i 1) w/2) (node-odd t)))))]
        [else (if (fxzero? i) (cons t '()) #f)])))
        ;[else (if (fxzero? i) t (error 'lookup-tree "illegal index"))])))

  (define lookup
    (lambda (i ls)
      (cond
        [(null? ls) #f]
        ;[(null? ls) (error 'k:lookup "illegal index")]
        [else (let [(t (car ls))]
                (if (fx< i (car t))
                    (lookup-tree (car t) i (cdr t))
                    (lookup (fx- i (car t)) (cdr ls))))])))

  (define update-tree
    (lambda (w i v t)
      (cond
        [(node? t)
         (if (fxzero? i) (node v (node-even t) (node-odd t))
             (let [(w/2 (shift w))]
               (if (fx<= i w/2)
                   (node (node-val t)
                              (update-tree w/2 (fx- i 1) v (node-even t))
                              (node-odd t))
                   (node (node-val t)
                              (node-even t)
                              (update-tree w/2 (fx- (fx- i 1) w/2) v (node-odd t))))))]
        [else (if (fxzero? i) v (error 'update-tree "illegal index"))])))

  (define update
    (lambda (i v ls)
      (cond
        [(null? ls) (error 'k:update "illegal index ~s ~s" i v)]
        [else (let [(t (car ls))]
                (if (fx< i (car t))
                    (cons `(,(car t) . ,(update-tree (car t) i v (cdr t))) (cdr ls))
                    (cons t (update (fx- i (car t)) v (cdr ls)))))])))
#| end |#

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
    ((rhs p) (cdr p))))

(define-syntax lhs
  (syntax-rules ()
    ((lhs p) (car p))))

(define unify
  (lambda (v^ w^ s)
    (let ([v (walk v^ s)]
          [w (walk w^ s)])
      (cond
        [(eq? v w) s]
        [(var? v) (ext-s v w s)]
        [(var? w) (ext-s w v s)]
        [(and (pair? v) (pair? w))
          (let ((s (unify (car v) (car w) s)))
            (and s (unify (cdr v) (cdr w) s)))]
        [(equal? v w) s]
        [else #f]))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g ...)
     (lambdag@ (s)
               (let ((x (walk* x s)) ...)
                 ((all g ...) s))))))

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
               ((exist (x) g0 g ... (λ (a) (list (reify^ x a))))
                empty-s)))]))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

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

(define (unify-check u v s)
  (let ([u (walk u s)]
        [v (walk v s)])
    ;;(let ((u (safe-walk u s))
    ;;(v (safe-walk v s)))
    (cond
      [(eq? u v) s]
      [(var? u) (ext-s-check u v s)]
      [(var? v) (ext-s-check v u s)]
      [(and (pair? u) (pair? v))
        (let ([s (unify-check (car u) (car v) s)])
          (and s (unify-check (cdr u) (cdr v) s)))]
      [(equal? u v) s]
      [else #f])))

(define (ext-s-check x v s)
  (cond
    [(occurs-check x v s) #f]
    [else (ext-s x v s)]))

(define (occurs-check x v s)
  (let ([v (safe-walk v s)])
    (cond
      [(var? v) (eq? v x)]
      [(pair? v)
       (or
         (occurs-check x (car v) s)
         (occurs-check x (cdr v) s))]
      [else #f])))

(define (walk* w s)
  (let ([v (safe-walk w s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons
         (walk* (car v) s)
         (walk* (cdr v) s))]
      [else v])))

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (ext-s v (reify-name (size-s s)) s))
        ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define reify
  (lambda (v)
    (walk* v (reify-s v empty-s))))

(define (reify^ v s)
  (walk* v (reify-s v s)))

#;
(define-syntax run  
  (syntax-rules ()
    ((_ n^ (x) g ...)
     (let ((n n^) (x (var 'x)))
       (if (or (not n) (> n 0))
         (map-inf n
           (lambda (s) (reify (walk* x s)))
           ((all g ...) empty-s))
         '())))))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e on-zero ((a^) on-one) ((a f) on-choice))
     (let ((a-inf e))
       (cond
         ((not a-inf) on-zero)
         ((not (and 
                 (pair? a-inf)
                 (procedure? (cdr a-inf))))
          (let ((a^ a-inf))
            on-one))
         (else (let ((a (car a-inf))
                     (f (cdr a-inf)))
                 on-choice)))))))

(define-syntax mzero
  (syntax-rules ()
    ((_) #f)))

(define-syntax unit
  (syntax-rules ()
    ((_ a) a)))

(define-syntax choice 
  (syntax-rules ()
    ((_ a f) (cons a f))))

(define map-inf
  (lambda (n p a-inf)
    (case-inf a-inf
      '()
      ((a) 
       (cons (p a) '()))
      ((a f) 
       (cons (p a)
         (cond
           ((not n) (map-inf n p (f)))
           ((> n 1) (map-inf (- n 1) p (f)))
           (else '())))))))

(define == 
  (lambda (v w)
    (lambdag@ (s)
      (cond
        ((unify v w s) => succeed)
        (else (fail s))))))

(define ==-check
  (lambda (v w)
    (lambdag@ (s)
      (cond
        ((unify-check v w s) => succeed)
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
    ((_) fail)
    ((_ (else g0 g ...)) (all g0 g ...))
    ((_ (g0 g ...) c ...)
     (ifu g0 (all g ...) (condu c ...)))))

(define-syntax ifa
  (syntax-rules ()
    ((_ g0 g1 g2)
     (lambdag@ (s)
       (let ((s-inf (g0 s)) (g^ g1))
         (case-inf s-inf
           (g2 s)
           ((s) (g^ s))
           ((s f) (bind s-inf g^))))))))

(define-syntax ifu
  (syntax-rules ()
    ((_ g0 g1 g2)
     (lambdag@ (s)
       (let ((s-inf (g0 s)) (g^ g1))
         (case-inf s-inf
           (g2 s)
           ((s) (g^ s))
           ((s f) (g^ s))))))))

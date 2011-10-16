#lang racket/base
(require "skew-bral.rkt")
(provide
  lambdaf@ lambdag@
    unify-check-sv unify-check-mv unify-sv unify-mv
    size-s
    run* var?
    empty-s ext-s-check occurs-check
    walk* reify-s reify-name reify ==-check mzero inc choice
    case-inf run take == bind* bind unify
    conde mplus* mplus conda ifa condu ifu project succeed fail
    walk exist ext-s safe-walk unify-check unify-safe
    )

(define-syntax lambdag@
  (syntax-rules ()
    ((_ (p) e) (lambda (p) e))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

;;;----------------------------------------------------------
;;; Various versions of walk for comparison testing.
;;;----------------------------------------------------------

;;-- O(n) algorithms -----------------------------------------

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

;;-------------------------------------------------------------------

(define (unify-check-sv u v s)
  (let ([u (walk u s)]
        [v (walk v s)])
    ;;(let ((u (safe-walk u s))
    ;;(v (safe-walk v s)))
    (cond
      [(eq? u v) s]
      [(var? u) (ext-s-check u v s)]
      [(var? v) (ext-s-check v u s)]
      [(and (pair? u) (pair? v))
        (let ([s (unify-check-sv (car u) (car v) s)])
          (and s (unify-check-sv (cdr u) (cdr v) s)))]
      [(equal? u v) s]
      [else #f])))

(define unify-check-mv
  (lambda (u v s)
    (let*-values ([(u s^) (walk u s)]
                  [(v s) (walk v s^)])
      ;;(let ((u (safe-walk u s))
      ;;(v (safe-walk v s)))
      (cond
        ((eq? u v) s)
        ((var? u) (ext-s-check u v s))
        ((var? v) (ext-s-check v u s))
        ((and (pair? u) (pair? v))
          (let ((s (unify-check-mv
                    (car u) (car v) s)))
            (and s (unify-check-mv
                    (cdr u) (cdr v) s))))
        ((equal? u v) s)
        (else #f)))))

(define ext-s-check
  (lambda (x v s)
    (cond
      [(occurs-check x v s) #f]
      [else (ext-s x v s)])))

(define occurs-check
  (lambda (x v s)
    (let ([v (safe-walk v s)])
      (cond
        [(var? v) (eq? v x)]
        [(pair? v)
          (or
            (occurs-check x (car v) s)
            (occurs-check x (cdr v) s))]
        [else #f]))))

(define walk*
  (lambda (w s)
    (let ((v (safe-walk w s)))
      (cond
        ((var? v) v)
        ((pair? v)
          (cons
            (walk* (car v) s)
            (walk* (cdr v) s)))
        (else v)))))

(define reify-s
  (lambda (v s)
    (let ((v (safe-walk v s)))
      (cond
        ((var? v)
          (ext-s v (reify-name (size-s s)) s))
        ((pair? v) (reify-s (cdr v)
                            (reify-s (car v) s)))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define rc -1)
(define rl '())
(define reify
  (lambda (v s)
    (set! rc -1)
    (set! rl '())
    (let loop [(v v)]
      (let ((v (safe-walk v s)))
        (cond
          ((assq v rl) => (lambda (a) (cdr a)))
          ((var? v) (begin (set! rc (add1 rc))
                            (let [(n (cons v (reify-name rc)))]
                              (set! rl (cons n rl))
                              (cdr n))))
          ((pair? v)
            (let [(c (loop (car v)))]
              (cons c (loop (cdr v)))))
          (else v))))))

(define ==-check
  (lambda (v w)
    (lambdag@ (a)
      (unify-check v w a))))

(define mzero #f)

(define-syntax inc
  (syntax-rules () ((_ e) (lambdaf@ () e))))

(define choice cons)

(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((a^) e2) ((a f) e3))
      (let ((a-inf e))
        (cond
          ((not a-inf) e0)
          ((procedure? a-inf)  (let ((f^ a-inf)) e1))
          ((not (and (pair? a-inf)
                     (procedure? (cdr a-inf))))
           (let ((a^ a-inf)) e2))
          (else (let ((a (car a-inf)) (f (cdr a-inf)))
                  e3)))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
      (take n
            (inc
              ((exist (x) g0 g ...
                      (lambdag@ (a)
                        (cons (reify x a) '())))
              empty-s))))))

(define take
  (lambda (n f)
    (if (and n (zero? n))
        '()
        (case-inf (f)
                  (() '())
                  ((f) (take n f))
                  ((a) a)
                  ((a f)
                    (cons (car a)
                          (take (and n (- n 1)) f)))))))

(define ==
  (lambda (u v)
    (lambdag@ (a)
      (unify u v a))))

(define-syntax exist
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
      (lambdag@ (s)
        (inc
          (let*-values ([(x s) (k:new-var 'x s)] ...)
            (bind* (g0 s) g ...)))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

(define bind
  (lambda (a-inf g)
    (case-inf a-inf
              (() mzero)
              ((f) (inc (bind (f) g)))
              ((a) (g a))
              ((a f) (mplus (g a) (inc (bind (f) g)))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
      (lambdag@ (a)
        (inc
          (mplus*
            (bind* (g0 a) g ...)
            (bind* (g1 a) g^ ...) ...))))))

(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus e0
                          (inc (mplus* e ...))))))

(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
              (() (f))
              ((f^) (inc (mplus (f) f^)))
              ((a) (choice a f))
              ((a f^) (choice a (inc (mplus (f) f^)))))))

(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
      (lambdag@ (a)
        (inc
        (ifa ((g0 a) g ...)
              ((g1 a) g^ ...) ...))))))

(define-syntax ifa
  (syntax-rules ()
    ((_) mzero)
    ((_ (e g ...) b ...)
      (let loop ((a-inf e))
        (case-inf a-inf
                  (() (ifa b ...))
                  ((f) (inc (loop (f))))
                  ((a) (bind* a-inf g ...))
                  ((a f) (bind* a-inf g ...)))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
      (lambdag@ (a)
        (inc
          (ifu ((g0 a) g ...)
               ((g1 a) g^ ...) ...))))))

(define-syntax ifu
  (syntax-rules ()
    ((_) mzero)
    ((_ (e g ...) b ...)
      (let loop ((a-inf e))
        (case-inf a-inf
                  (() (ifu b ...))
                  ((f) (inc (loop (f))))
                  ((a) (bind* a-inf g ...))
                  ((a f) (bind* a g ...)))))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
      (lambdag@ (s)
        (let ((x (walk* x s)) ...)
          ((exist () g g* ...) s))))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define onceo
  (lambda (g)
    (condu
      (g succeed)
      (succeed fail)))) ;;<< why does the 'else' fail in ikarus?

(define unify-sv
  (lambda (v^ w^ s)
    (let ([v (walk v^ s)]
          [w (walk w^ s)])
      (cond
        [(eq? v w) s]
        [(var? v) (ext-s v w s)]
        [(var? w) (ext-s w v s)]
        [(and (pair? v) (pair? w))
          (let ((s (unify-sv (car v) (car w) s)))
            (and s (unify-sv (cdr v) (cdr w) s)))]
        [(equal? v w) s]
        [else #f]))))

(define unify-mv
  (lambda (v w s)
    (let*-values ([(v s^) (walk v s)]
                  [(w s) (walk w s^)])
      (cond
        ((eq? v w) s)
        ((var? v) (ext-s v w s))
        ((var? w) (ext-s w v s))
        ((and (pair? v) (pair? w))
          (let ((s (unify-mv (car v) (car w) s)))
            (and s (unify-mv (cdr v) (cdr w) s))))
        ((equal? v w) s)
        (else #f)))))

(define unify-safe
  (lambda (v w s)
    (let ([v (safe-walk v s)]
          [w (safe-walk w s)])
      (cond
        [(eq? v w) s]
        [(var? v) (ext-s v w s)]
        [(var? w) (ext-s w v s)]
        [(and (pair? v) (pair? w))
          (let ((s (unify-safe (car v) (car w) s)))
            (and s (unify-safe (cdr v) (cdr w) s)))]
        [(equal? v w) s]
        [else #f]))))

;;-------------------------------------------------------------------
;; choose versions of various functions/macros

(define empty-s k:empty)

(define ext-s k:update)

(define size-s k:size)

(define safe-walk walk)

(define-syntax unify-check
  (syntax-rules ()
    [(_ u v s) (unify-check-sv u v s)]))

(define-syntax unify
  (syntax-rules ()
    [(_ v w s) (unify-sv v w s)]))

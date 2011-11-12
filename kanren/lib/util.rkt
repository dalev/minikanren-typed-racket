#lang racket/base
(require "../main.rkt")

(provide (all-defined-out))

(define (once g) (condu [g succeed] [else fail]))

(define anyo
  (lambda (g)
    (conde
      (g succeed)
      (else (anyo g)))))

(define nevero (anyo fail))
 
(define alwayso (anyo succeed))

;;;  'trace-vars' can be used to print the values of selected variables
;;;  in the substitution.
(define-syntax trace-vars
  (syntax-rules ()
    ((_ title x ...)
     (lambdag@ (s)
       (begin
         (printf "~a~n" title)
         (for-each (lambda (x_ t) 
                     (printf "~a = ~s~n" x_ t))
           `(x ...) (reify (walk* `(,x ...) s)))
         (unit s))))))

;;; (run* (q)
;;;   (exist (r)
;;;     (== 3 q)
;;;     (trace-vars "What it is!" q r)))
;;;
;;; What it is!
;;; q = 3
;;; r = _.0
;;; (3)

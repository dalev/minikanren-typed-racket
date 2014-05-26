#lang racket/base
(require "kanren.rkt")

(provide (all-defined-out))

(define (%repeat g)
  (conde
    [g succeed]
    [succeed (%repeat g)]))

(define %never (%repeat fail))
 
(define %always (%repeat succeed))

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
;;;   (fresh (r)
;;;     (== 3 q)
;;;     (trace-vars "What it is!" q r)))
;;;
;;; What it is!
;;; q = 3
;;; r = _.0
;;; (3)

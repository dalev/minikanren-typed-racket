#lang racket/base
(require (for-syntax racket/base syntax/parse))

(provide in-tails)

;; CR dalev: implement for functional contexts
(define (in-tails/proc xs)
  (error 'in-tails/proc "Unimplemented"))

(define-sequence-syntax in-tails
  (λ () #'in-tails/proc)
  (λ (stx)
    (syntax-parse stx
      [[(v) (_ xs-expr)]
       #'[(v) (:do-in 
                ([(xs) xs-expr])
                (unless (list? xs)
                  (raise-type-error 'in-tails "list" xs))
                ([ys xs])
                #t
                ([(v) (values ys)])
                #t
                (not (null? ys))
                [(cdr ys)])]])))

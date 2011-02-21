#lang typed/racket/base
(require (except-in racket/list
                    partition))
(require/typed racket/list
  [partition (All (a) ((a -> Boolean) (Listof a) -> (values (Listof a) (Listof a))))])

(provide rev-append
         map
         partition
         (all-from-out racket/list))

(: rev-append (All (a) ((Listof a) (Listof a) * -> (Listof a))))
(define (rev-append xs . more-lists)
  (for/fold: : (Listof a) ([result : (Listof a) xs]) ([ys (in-list more-lists)])
    (for/fold: : (Listof a) ([result : (Listof a) result]) ([y (in-list ys)])
      (cons y result))))


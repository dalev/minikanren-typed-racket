#lang typed/racket/base
(require racket/list)
(provide rev-append
         (all-from-out racket/list))

(: rev-append (All (a) ((Listof a) (Listof a) * -> (Listof a))))
(define (rev-append xs . more-lists)
  (for/fold: : (Listof a) ([result : (Listof a) xs]) ([ys (in-list more-lists)])
    (for/fold: : (Listof a) ([result : (Listof a) result]) ([y (in-list ys)])
      (cons y result))))


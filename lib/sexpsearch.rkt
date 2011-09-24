#lang racket/base
(require racklog)

(provide %anywhere
         %children)

;; CR dalev: recur into more compound objects (vectors / structs)
(define %anywhere
  (%rel (sexp s t pattern)
    [(sexp pattern) (%= sexp pattern)]
    [((cons s t) pattern)
     (%or (%anywhere s pattern)
          (%anywhere t pattern))]))

(define %children
  (%rel (s t pattern)
    [((cons s t) pattern)
     (%or (%= s pattern)
          (%children t pattern))]))

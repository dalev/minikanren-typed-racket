#lang racket/base
(require "kanren.rkt"
         "list.rkt")

(provide %anywhere)

(define (%anywhere sexp %pred)
  (conde
    [(%pred sexp) succeed]
    [succeed
      (fresh (fst snd)
        (%cons sexp fst snd)
        (any (%anywhere fst %pred)
             (%anywhere snd %pred)))]))

(define (%sexp-mem sexp elt)
  (conde 
    [(%null sexp) (%null elt)]
    [(== sexp elt) succeed]
    [succeed
     (fresh (fst snd)
       (%cons sexp fst snd)
       (any (%sexp-mem fst elt)
            (%sexp-mem snd elt)))]))

